// -*- mode: c; tab-width: 8; indent-tabs-mode: 1; st-rulers: [70] -*-
// vim: ts=8 sw=8 ft=c noet

/*
 * (The MIT License)
 *
 * Copyright (c) 2013 Andrew Bennett
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * 'Software'), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "hierdis_port.h"

hierdis_port_t *
hierdis_port_new(ErlDrvPort drv_port)
{
	hierdis_port_t *port;

	port = (hierdis_port_t *)(driver_alloc(sizeof (hierdis_port_t)));
	if (port == NULL) {
		return port;
	}

	port->context = NULL;
	port->drv_port = drv_port;
	port->term_port = driver_mk_port(drv_port);
	port->err_atom = (sds)NULL;
	port->err_reason = (sds)NULL;
	port->timeout = (unsigned long)0;
	port->opened = 0;
	port->flags = 0;
	port->fixflags = 0;

	return port;
}

void
hierdis_port_free(hierdis_port_t *port)
{
	if (port == NULL) {
		return;
	}
	if (port->context != NULL) {
		port->context = NULL;
	}
	if (port->err_atom != NULL) {
		(void) sdsfree(port->err_atom);
	}
	if (port->err_reason != NULL) {
		(void) sdsfree(port->err_reason);
	}
	(void) driver_free(port);
	port = NULL;
}

void
hierdis_port_stop(hierdis_port_t *port)
{
	if (port == NULL) {
		return;
	}
	if (port->context == NULL) {
		(void) hierdis_port_free(port);
		return;
	}
	(void) redisAsyncDisconnect(port->context);
}

void
hierdis_port_read(hierdis_port_t *port)
{
	(void) redisAsyncHandleRead(port->context);
}

void
hierdis_port_write(hierdis_port_t *port)
{
	(void) redisAsyncHandleWrite(port->context);
}

void
hierdis_port_timeout(hierdis_port_t *port)
{
	struct redisAsyncContext *context;

	if (port == NULL) {
		return;
	}
	if (port->opened == 1) {
		return;
	}

	context = port->context;
	port->context->data = NULL;
	port->context = NULL;
	(void) hierdis_port_open_timeout(context, port);
	(void) hierdis_port_closed(port);
	driver_failure_eof(port->drv_port);
}

void
hierdis_port_on_connect(const struct redisAsyncContext *context, int status)
{
	hierdis_port_t *port;
	redisErlDriverEvents *p;

	(void) status; // Unused

	port = (hierdis_port_t *)(context->data);
	port->opened = 1;
	port->flags = context->c.flags;
	port->flags &= ~REDIS_IN_CALLBACK;

	driver_cancel_timer(port->drv_port);

	if ((port->flags & REDIS_CONNECTED) > 0) {
		(void) hierdis_port_opened(port);
	} else {
		if (port->context != NULL && port->context->ev.data != NULL) {
			p = (redisErlDriverEvents *)(port->context->ev.data);
			p->kill = 1;
		}
		port->context->data = NULL;
		port->context = NULL;
		(void) hierdis_port_open_error((struct redisAsyncContext *)context, port);
		(void) hierdis_port_closed(port);
	}
}

void
hierdis_port_on_disconnect(const struct redisAsyncContext *context, int status)
{
	hierdis_port_t *port;

	(void) status; // Unused

	port = (hierdis_port_t *)(context->data);
	port->flags = context->c.flags;
	port->flags &= ~REDIS_IN_CALLBACK;

	port->context->data = NULL;
	port->context = NULL;

	if (context->err || context->c.err) {
		(void) hierdis_port_close_error((struct redisAsyncContext *)context, port);
		(void) hierdis_port_closed(port);
		driver_failure_eof(port->drv_port);
	} else {
		(void) hierdis_port_closed(port);
		driver_failure_eof(port->drv_port);
	}
}

void
hierdis_port_on_reply(struct redisAsyncContext *context, void *replydata, void *privdata)
{
	redisReply *reply;
	redisContext *c;
	hierdis_port_t *port;
	int subscribed;

	(void) privdata; // Unused

	if (replydata == NULL) {
		return; // Ignore empty replies
	}

	subscribed = 0;

	reply = (redisReply *)(replydata);
	port = (hierdis_port_t *)(context->data);
	c = &(context->c);

	if (port->flags & REDIS_SUBSCRIBED) {
		subscribed = 1;
	}

	if (port->fixflags == 1) {
		port->fixflags = 0;
		c->flags &= ~REDIS_SUBSCRIBED;
	}

	port->flags = context->c.flags;
	port->flags &= ~REDIS_IN_CALLBACK;

	if (port->flags & REDIS_SUBSCRIBED) {
		subscribed = 1;
	}

	if (subscribed == 1 && !(c->flags & REDIS_SUBSCRIBED || c->flags & REDIS_MONITORING)) {
		port->fixflags = 1;
		c->flags |= REDIS_SUBSCRIBED;
	}

	(void) hierdis_port_respond(port, reply, subscribed);
}

static hierdis_port_spec_t *
spec_new(struct redisAsyncContext *context, hierdis_port_t *port)
{
	hierdis_port_spec_t *spec;

	spec = (hierdis_port_spec_t *)(driver_alloc(sizeof (hierdis_port_spec_t)));
	if (spec == NULL) {
		return spec;
	}
	spec->size = (ErlDrvSizeT)0;
	spec->data = NULL;
	spec->index = 0;
	spec->port = port;
	spec->context = context;

	return spec;
}

static void
spec_free(hierdis_port_spec_t *spec)
{
	(void) driver_free(spec);
	spec = NULL;
	return;
}

static int
fix_spec(hierdis_port_spec_t *spec, ErlDrvSizeT new_size)
{
	if (new_size >= spec->size) {
		spec->size = ((new_size >> 5) + 1) << 5;
		if (spec->data == NULL) {
			spec->data = (ErlDrvTermData *)(driver_alloc(spec->size * (sizeof(ErlDrvTermData))));
			if (spec->data == NULL) {
				return -1;
			}
		} else {
			spec->data = (ErlDrvTermData *)(driver_realloc((void *)(spec->data), spec->size * (sizeof(ErlDrvTermData))));
			if (spec->data == NULL) {
				return -1;
			}
		}
	}

	return 0;
}

static ErlDrvTermData
hierdis_port_get_redis_error(int code)
{
	ErlDrvTermData atom;
	sds atom_code;

	atom_code = NULL;

	switch(code) {
	case REDIS_REPLY_ERROR:
		atom = HI_ATOM(redis_reply_error);
		break;
	case REDIS_ERR_IO:
		atom = HI_ATOM(redis_err_io);
		break;
	case REDIS_ERR_EOF:
		atom = HI_ATOM(redis_err_eof);
		break;
	case REDIS_ERR_PROTOCOL:
		atom = HI_ATOM(redis_err_protocol);
		break;
	case REDIS_ERR_OOM:
		atom = HI_ATOM(redis_err_oom);
		break;
	case REDIS_ERR_OTHER:
		atom = HI_ATOM(redis_err_other);
		break;
	default:
		atom_code = sdscatprintf(sdsempty(), "redis_err_code_%d", code);
		if (atom_code == NULL) {
			return (ErlDrvTermData)NULL;
		}
		atom = driver_mk_atom((char *)atom_code);
		(void) sdsfree(atom_code);
		break;
	}

	return atom;
}

static void
hierdis_port_closed(hierdis_port_t *port)
{
	hierdis_port_spec_t *spec;

	spec = NULL;

	spec = spec_new((struct redisAsyncContext *)NULL, port);
	if (spec == NULL) {
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	if (fix_spec(spec, (ErlDrvSizeT)(spec->index + 6)) < 0) {
		HI_FAIL_OOM(port->drv_port);
		return;
	}
	spec->data[spec->index++] = ERL_DRV_ATOM;
	spec->data[spec->index++] = HI_ATOM(redis_closed);
	spec->data[spec->index++] = ERL_DRV_PORT;
	spec->data[spec->index++] = spec->port->term_port;
	spec->data[spec->index++] = ERL_DRV_TUPLE;
	spec->data[spec->index++] = 2;

	(void) hierdis_port_output(spec);
}

static void
hierdis_port_close_error(struct redisAsyncContext *context, hierdis_port_t *port)
{
	ErlDrvTermData err;
	char *errstr;
	int errlen;
	hierdis_port_spec_t *spec;

	spec = spec_new((struct redisAsyncContext *)context, port);
	if (spec == NULL) {
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	if (fix_spec(spec, (ErlDrvSizeT)(spec->index + 13)) < 0) {
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	spec->data[spec->index++] = ERL_DRV_ATOM;
	spec->data[spec->index++] = HI_ATOM(redis_error);
	spec->data[spec->index++] = ERL_DRV_PORT;
	spec->data[spec->index++] = spec->port->term_port;

	if (context->err) {
		err = hierdis_port_get_redis_error(context->err);
	} else if (context->c.err) {
		err = hierdis_port_get_redis_error(context->c.err);
	} else {
		err = HI_ATOM(redis_err_other);
	}

	if (context->errstr) {
		errstr = context->errstr;
	} else if (context->c.errstr) {
		errstr = context->c.errstr;
	} else {
		errstr = "ERR unknown close error";
	}

	errlen = strlen(errstr);

	spec->data[spec->index++] = ERL_DRV_ATOM;
	spec->data[spec->index++] = err;
	spec->data[spec->index++] = ERL_DRV_STRING;
	spec->data[spec->index++] = (ErlDrvTermData)(errstr);
	spec->data[spec->index++] = errlen;
	spec->data[spec->index++] = ERL_DRV_TUPLE;
	spec->data[spec->index++] = 2;

	spec->data[spec->index++] = ERL_DRV_TUPLE;
	spec->data[spec->index++] = 3;

	(void) hierdis_port_output(spec);
}

static void
hierdis_port_opened(hierdis_port_t *port)
{
	hierdis_port_spec_t *spec;

	spec = NULL;

	spec = spec_new((struct redisAsyncContext *)NULL, port);
	if (spec == NULL) {
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	if (fix_spec(spec, (ErlDrvSizeT)(spec->index + 6)) < 0) {
		HI_FAIL_OOM(port->drv_port);
		return;
	}
	spec->data[spec->index++] = ERL_DRV_ATOM;
	spec->data[spec->index++] = HI_ATOM(redis_opened);
	spec->data[spec->index++] = ERL_DRV_PORT;
	spec->data[spec->index++] = spec->port->term_port;
	spec->data[spec->index++] = ERL_DRV_TUPLE;
	spec->data[spec->index++] = 2;

	(void) hierdis_port_output(spec);
}

static void
hierdis_port_open_error(struct redisAsyncContext *context, hierdis_port_t *port)
{
	ErlDrvTermData err;
	char *errstr;
	int errlen;
	hierdis_port_spec_t *spec;

	spec = NULL;

	spec = spec_new((struct redisAsyncContext *)context, port);
	if (spec == NULL) {
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	if (fix_spec(spec, (ErlDrvSizeT)(spec->index + 13)) < 0) {
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	spec->data[spec->index++] = ERL_DRV_ATOM;
	spec->data[spec->index++] = HI_ATOM(redis_error);
	spec->data[spec->index++] = ERL_DRV_PORT;
	spec->data[spec->index++] = spec->port->term_port;

	if (context->err) {
		err = hierdis_port_get_redis_error(context->err);
	} else if (context->c.err) {
		err = hierdis_port_get_redis_error(context->c.err);
	} else {
		err = HI_ATOM(redis_err_other);
	}

	if (context->errstr) {
		errstr = context->errstr;
	} else if (context->c.errstr) {
		errstr = context->c.errstr;
	} else {
		errstr = "ERR unknown open error";
	}

	errlen = strlen(errstr);

	spec->data[spec->index++] = ERL_DRV_ATOM;
	spec->data[spec->index++] = err;
	spec->data[spec->index++] = ERL_DRV_STRING;
	spec->data[spec->index++] = (ErlDrvTermData)(errstr);
	spec->data[spec->index++] = errlen;
	spec->data[spec->index++] = ERL_DRV_TUPLE;
	spec->data[spec->index++] = 2;

	spec->data[spec->index++] = ERL_DRV_TUPLE;
	spec->data[spec->index++] = 3;

	(void) hierdis_port_output(spec);
}

static void
hierdis_port_open_timeout(struct redisAsyncContext *context, hierdis_port_t *port)
{
	ErlDrvTermData err;
	char *errstr;
	int errlen;
	hierdis_port_spec_t *spec;

	spec = NULL;

	spec = spec_new((struct redisAsyncContext *)context, port);
	if (spec == NULL) {
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	if (fix_spec(spec, (ErlDrvSizeT)(spec->index + 13)) < 0) {
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	spec->data[spec->index++] = ERL_DRV_ATOM;
	spec->data[spec->index++] = HI_ATOM(redis_error);
	spec->data[spec->index++] = ERL_DRV_PORT;
	spec->data[spec->index++] = spec->port->term_port;

	err = HI_ATOM(redis_err_timeout);

	errstr = sdscatprintf(sdsempty(), "ERR Timeout after %" PRIu32 "ms while attempting to connect.", (uint32_t)port->timeout);

	errlen = strlen(errstr);

	spec->data[spec->index++] = ERL_DRV_ATOM;
	spec->data[spec->index++] = err;
	spec->data[spec->index++] = ERL_DRV_STRING;
	spec->data[spec->index++] = (ErlDrvTermData)(errstr);
	spec->data[spec->index++] = errlen;
	spec->data[spec->index++] = ERL_DRV_TUPLE;
	spec->data[spec->index++] = 2;

	spec->data[spec->index++] = ERL_DRV_TUPLE;
	spec->data[spec->index++] = 3;

	(void) hierdis_port_output(spec);
}

static void
hierdis_port_respond(hierdis_port_t *port, redisReply *reply, int subscribed)
{
	hierdis_port_spec_t *spec;

	spec = NULL;

	spec = spec_new(port->context, port);
	if (spec == NULL) {
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	if (fix_spec(spec, (ErlDrvSizeT)(spec->index + 4)) < 0) {
		(void) spec_free(spec);
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	spec->data[spec->index++] = ERL_DRV_ATOM;
	if (reply->type == REDIS_REPLY_ERROR) {
		spec->data[spec->index++] = HI_ATOM(redis_error);
	} else {
		if (subscribed == 1) {
			spec->data[spec->index++] = HI_ATOM(redis_message);
		} else {
			spec->data[spec->index++] = HI_ATOM(redis_reply);
		}
	}
	spec->data[spec->index++] = ERL_DRV_PORT;
	spec->data[spec->index++] = port->term_port;

	if (hierdis_port_make_term_from_reply(reply, spec) < 0) {
		(void) spec_free(spec);
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	if (fix_spec(spec, (ErlDrvSizeT)(spec->index + 2)) < 0) {
		(void) spec_free(spec);
		HI_FAIL_OOM(port->drv_port);
		return;
	}

	spec->data[spec->index++] = ERL_DRV_TUPLE;
	spec->data[spec->index++] = 3;

	(void) hierdis_port_output(spec);
}

static void
hierdis_port_output(hierdis_port_spec_t *spec)
{
	switch (erl_drv_output_term(spec->port->term_port, spec->data, spec->index)) {
	case -1: // error in input data
		(void) spec_free(spec);
		HI_FAIL_BADSPEC(spec->port->drv_port);
		break;
	case 0: // the message was not delivered (bad to pid or closed port)
	case 1: // the message was delivered successfully
	default:
		(void) spec_free(spec);
		break;
	}
}

static int
hierdis_port_make_term_from_reply(redisReply *reply, hierdis_port_spec_t *spec)
{
	int retval;
	char *errstr;
	int errlen;

	retval = -1;

	switch (reply->type) {
	case REDIS_REPLY_STRING:
	case REDIS_REPLY_STATUS:
		retval = hierdis_port_make_binary_from_reply(reply, spec);
		break;
	case REDIS_REPLY_ARRAY:
		retval = hierdis_port_make_list_from_reply(reply, spec);
		break;
	case REDIS_REPLY_INTEGER:
		retval = hierdis_port_make_integer_from_reply(reply, spec);
		break;
	case REDIS_REPLY_NIL:
		if (fix_spec(spec, (ErlDrvSizeT)(spec->index + 2)) < 0) {
			retval = -1;
		} else {
			spec->data[spec->index++] = ERL_DRV_ATOM;
			spec->data[spec->index++] = HI_ATOM(undefined);
			retval = spec->index;
		}
		break;
	default:
		if (fix_spec(spec, (ErlDrvSizeT)(spec->index + 7)) < 0) {
			retval = -1;
		} else {
			if (reply->str == NULL) {
				errstr = "ERR unknown reply error";
				errlen = (int)(strlen(errstr));
			} else {
				errstr = reply->str;
				errlen = reply->len;
			}
			spec->data[spec->index++] = ERL_DRV_ATOM;
			spec->data[spec->index++] = HI_ATOM(redis_reply_error);
			spec->data[spec->index++] = ERL_DRV_STRING;
			spec->data[spec->index++] = (ErlDrvTermData)errstr;
			spec->data[spec->index++] = errlen;
			spec->data[spec->index++] = ERL_DRV_TUPLE;
			spec->data[spec->index++] = 2;
			retval = spec->index;
		}
		break;
	}

	return retval;
}

static int
hierdis_port_make_binary_from_reply(redisReply *reply, hierdis_port_spec_t *spec)
{
	if (fix_spec(spec, (ErlDrvSizeT)(spec->index + 3)) < 0) {
		return -1;
	}
	spec->data[spec->index++] = ERL_DRV_BUF2BINARY;
	spec->data[spec->index++] = (ErlDrvTermData)reply->str;
	spec->data[spec->index++] = reply->len;
	return spec->index;
}

static int
hierdis_port_make_list_from_reply(redisReply *reply, hierdis_port_spec_t *spec)
{
	int arity;

	if (fix_spec(spec, (ErlDrvSizeT)(spec->index + reply->elements + 3)) < 0) {
		return -1;
	}

	for (arity = 0; arity < reply->elements; arity++) {
		hierdis_port_make_term_from_reply(reply->element[arity], spec);
	}
	spec->data[spec->index++] = ERL_DRV_NIL;
	spec->data[spec->index++] = ERL_DRV_LIST;
	spec->data[spec->index++] = arity + 1;
	return spec->index;
}

static int
hierdis_port_make_integer_from_reply(redisReply *reply, hierdis_port_spec_t *spec)
{
	ei_x_buff *buf;

	if (fix_spec(spec, (ErlDrvSizeT)(spec->index + reply->elements + 3)) < 0) {
		return -1;
	}

	buf = (ei_x_buff *)(driver_alloc(sizeof (ei_x_buff)));
	if (buf == NULL) {
		return -1;
	}

	ei_x_new_with_version(buf);
	ei_x_encode_longlong(buf, (EI_LONGLONG)reply->integer);

	spec->data[spec->index++] = ERL_DRV_EXT2TERM;
	spec->data[spec->index++] = (ErlDrvTermData)buf->buff;
	spec->data[spec->index++] = buf->buffsz;
	return spec->index;
}
