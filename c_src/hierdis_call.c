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

#include "hierdis_call.h"

hierdis_call_t *
hierdis_call_new(hierdis_port_t *port, unsigned int command, char *buf, ErlDrvSizeT len,
		char *rbuf, ErlDrvSizeT rlen)
{
	hierdis_call_t *call;

	if (port == NULL) {
		return (hierdis_call_t *)NULL;
	}

	call = driver_alloc(sizeof (hierdis_call_t));
	if (call == NULL) {
		return call;
	}

	call->port = port;
	call->cmd = command;
	call->buf = buf;
	call->len = len;
	call->idx = 0;
	call->rbuf = rbuf;
	call->rlen = rlen;
	call->ridx = 0;
	call->olen = 0;

	return call;
}

void
hierdis_call_free(hierdis_call_t *call)
{
	if (call == NULL) {
		return;
	}
	(void) driver_free(call);
}

void
hierdis_call_execute(hierdis_call_t *call)
{
	int version;

	if (ei_decode_version(call->buf, &(call->idx), &version) < 0) {
		(void) hierdis_call_badarg(call);
		return;
	}

	if (call->port->err_atom != NULL) {
		(void) hierdis_call_error(call, call->port->err_atom, call->port->err_reason);
		return;
	}

	if (call->port->context == NULL) {
		switch (call->cmd) {
		case HIERDIS_CALL_CONNECT:
			(void) hierdis_call_connect(call);
			break;
		case HIERDIS_CALL_CONNECT_UNIX:
			(void) hierdis_call_connect_unix(call);
			break;
		case HIERDIS_CALL_COMMAND:
		case HIERDIS_CALL_DISCONNECT:
			(void) hierdis_call_error(call, HI_STRING(redis_err_context), "USE: connect/2, connect/3, connect_unix/1, connect_unix/2, controlling_process/2");
			break;
		default:
			(void) hierdis_call_badarg(call);
			break;
		}
	} else {
		switch (call->cmd) {
		case HIERDIS_CALL_CONNECT:
		case HIERDIS_CALL_CONNECT_UNIX:
			(void) hierdis_call_error(call, HI_STRING(redis_err_context), "USE: command/2, append_command/2, close/1, controlling_process/2");
			break;
		case HIERDIS_CALL_COMMAND:
			(void) hierdis_call_command(call);
			break;
		case HIERDIS_CALL_DISCONNECT:
			(void) hierdis_call_disconnect(call);
			break;
		default:
			(void) hierdis_call_badarg(call);
			break;
		}
	}
}

static void
hierdis_call_connect(hierdis_call_t *call)
{
	char *buffer;
	int *index;
	int arity;
	int type;
	int type_length;
	char *host;
	unsigned long port;
	unsigned long timeout;

	buffer = call->buf;
	index = &(call->idx);

	if (ei_decode_tuple_header(buffer, index, &arity) < 0) {
		(void) hierdis_call_badarg(call);
		return;
	}

	if (arity < 2 || arity > 3) {
		(void) hierdis_call_badarg(call);
		return;
	}

	// Host
	ei_get_type(buffer, index, &type, &type_length);
	if (type != ERL_STRING_EXT) {
		(void) hierdis_call_badarg(call);
		return;
	}
	host = (char *)(driver_alloc((ErlDrvSizeT)(type_length + 1)));
	if (host == NULL) {
		(void) hierdis_call_badarg(call);
		return;
	}
	if (ei_decode_string(buffer, index, host) < 0) {
		(void) hierdis_call_badarg(call);
		return;
	}

	// Port
	if (ei_decode_ulong(buffer, index, &port) < 0) {
		(void) driver_free(host);
		(void) hierdis_call_badarg(call);
		return;
	}

	// Timeout
	if (arity == 3 && ei_decode_ulong(buffer, index, &timeout) < 0) {
		(void) driver_free(host);
		(void) hierdis_call_badarg(call);
		return;
	}

	call->port->context = redisAsyncConnect(host, port);
	(void) driver_free(host);
	if (call->port->context == NULL) {
		(void) hierdis_call_badarg(call);
		return;
	}

	call->port->context->data = (void *)(call->port);

	if (call->port->context->err) {
		(void) hierdis_call_redis_error(call, call->port->context->err, call->port->context->errstr);
		return;
	} else if (call->port->context->c.err) {
		(void) hierdis_call_redis_error(call, call->port->context->c.err, call->port->context->c.errstr);
		return;
	}

	(void) hierdis_call_attach(call, ((arity == 3) ? 1 : 0), timeout);
}

static void
hierdis_call_connect_unix(hierdis_call_t *call)
{
	char *buffer;
	int *index;
	int arity;
	int type;
	int type_length;
	char *path;
	unsigned long timeout;

	buffer = call->buf;
	index = &(call->idx);

	if (ei_decode_tuple_header(buffer, index, &arity) < 0) {
		(void) hierdis_call_badarg(call);
		return;
	}

	if (arity < 1 || arity > 2) {
		(void) hierdis_call_badarg(call);
		return;
	}

	// Path
	ei_get_type(buffer, index, &type, &type_length);
	if (type != ERL_STRING_EXT) {
		(void) hierdis_call_badarg(call);
		return;
	}
	path = (char *)(driver_alloc((ErlDrvSizeT)(type_length + 1)));
	if (path == NULL) {
		(void) hierdis_call_badarg(call);
		return;
	}
	if (ei_decode_string(buffer, index, path) < 0) {
		(void) hierdis_call_badarg(call);
		return;
	}

	// Timeout
	if (arity == 2 && ei_decode_ulong(buffer, index, &timeout) < 0) {
		(void) driver_free(path);
		(void) hierdis_call_badarg(call);
		return;
	}

	call->port->context = redisAsyncConnectUnix(path);
	(void) driver_free(path);
	if (call->port->context == NULL) {
		(void) hierdis_call_badarg(call);
		return;
	}

	call->port->context->data = (void *)(call->port);

	if (call->port->context->err) {
		(void) hierdis_call_redis_error(call, call->port->context->err, call->port->context->errstr);
		return;
	} else if (call->port->context->c.err) {
		(void) hierdis_call_redis_error(call, call->port->context->c.err, call->port->context->c.errstr);
		return;
	}

	(void) hierdis_call_attach(call, ((arity == 2) ? 1 : 0), timeout);
}

static void
hierdis_call_command(hierdis_call_t *call)
{
	char *buffer;
	int *index;
	unsigned int hiredis_argc;

	buffer = call->buf;
	index = &(call->idx);

	// Args
	if (ei_decode_list_header(buffer, index, (int *)&hiredis_argc) < 0) {
		(void) hierdis_call_badarg(call);
		return;
	}

	if (hiredis_argc < 1) {
		(void) hierdis_call_badarg(call);
		return;
	}

	const char *hiredis_argv[hiredis_argc];
	size_t hiredis_argv_lengths[hiredis_argc];

	(void) hierdis_call_command_execute(call, hiredis_argc, hiredis_argv, hiredis_argv_lengths);
}

static void
hierdis_call_disconnect(hierdis_call_t *call)
{
	int *length;
	char *buffer;
	int *index;

	length = &(call->olen);
	buffer = call->rbuf;
	index = &(call->ridx);

	if (call->port->context != NULL) {
		(void) redisAsyncDisconnect(call->port->context);
		call->port->context = NULL;
	}

	ei_encode_version(NULL, length);
	ei_encode_atom(NULL, length, HI_STRING(ok));

	if ((int)call->rlen < *length) {
		buffer = (char *)driver_realloc((void *)buffer, (ErlDrvSizeT)*length);
		call->rlen = (ErlDrvSizeT)*length;
	}

	ei_encode_version(buffer, index);
	ei_encode_atom(buffer, index, HI_STRING(ok));
}

static void
hierdis_call_attach(hierdis_call_t *call, int with_timeout, unsigned long timeout)
{
	int *length;
	char *buffer;
	int *index;

	length = &(call->olen);
	buffer = call->rbuf;
	index = &(call->ridx);

	if (redisErlDriverAttach(call->port->context, call->port->drv_port, (ErlDrvData)call->port) != REDIS_OK) {
		(void) redisAsyncDisconnect(call->port->context);
		call->port->context = NULL;
		(void) hierdis_call_badarg(call);
		return;
	}

	if (redisAsyncSetConnectCallback(call->port->context, hierdis_port_on_connect) != REDIS_OK) {
		(void) redisAsyncDisconnect(call->port->context);
		call->port->context = NULL;
		(void) hierdis_call_badarg(call);
		return;
	}

	if (redisAsyncSetDisconnectCallback(call->port->context, hierdis_port_on_disconnect) != REDIS_OK) {
		(void) redisAsyncDisconnect(call->port->context);
		call->port->context = NULL;
		(void) hierdis_call_badarg(call);
		return;
	}

	ei_encode_version(NULL, length);
	ei_encode_atom(NULL, length, HI_STRING(ok));

	if ((int)call->rlen < *length) {
		buffer = (char *)driver_realloc((void *)buffer, (ErlDrvSizeT)*length);
		call->rlen = (ErlDrvSizeT)*length;
	}

	ei_encode_version(buffer, index);
	ei_encode_atom(buffer, index, HI_STRING(ok));

	if (with_timeout == 1) {
		call->port->timeout = timeout;
		if (driver_set_timer(call->port->drv_port, timeout) < 0) {
			(void) hierdis_call_badarg(call);
			return;
		}
	}
}

/*
 * Command Args functions
 */
static sds
hierdis_call_decode_iolist(char *buffer, int *index, sds term)
{
	int type;
	int size;
	int arity;
	char *byte;
	char *string;
	int i;

	if (term == NULL) {
		return term;
	}

	if (sdslen(term) > 0) {
		byte = NULL;
		byte = (char *)(driver_alloc((ErlDrvSizeT)(sizeof (char))));
		if (byte == NULL) {
			(void) sdsfree(term);
			return (sds)NULL;
		}

		if (ei_decode_char(buffer, index, byte) == 0) {
			term = sdscatlen(term, byte, 1);
			return term;
		}
	}

	type = 0;
	size = 0;
	arity = 0;

	ei_get_type(buffer, index, &type, &size);

	switch (type) {
	case ERL_BINARY_EXT:
		string = NULL;
		string = (char *)(driver_alloc((ErlDrvSizeT)(size + 1)));
		if (string == NULL) {
			(void) sdsfree(term);
			return (sds)NULL;
		}
		if (ei_decode_binary(buffer, index, string, (long *)NULL) < 0) {
			(void) driver_free(string);
			(void) sdsfree(term);
			return (sds)NULL;
		}
		term = sdscatlen(term, string, size);
		(void) driver_free(string);
		return term;
	case ERL_STRING_EXT:
		string = NULL;
		string = (char *)(driver_alloc((ErlDrvSizeT)(size + 1)));
		if (string == NULL) {
			(void) sdsfree(term);
			return (sds)NULL;
		}
		if (ei_decode_string(buffer, index, string) < 0) {
			(void) driver_free(string);
			(void) sdsfree(term);
			return (sds)NULL;
		}
		term = sdscatlen(term, string, size);
		(void) driver_free(string);
		return term;
	case ERL_LIST_EXT:
		if (size < 1) {
			return term;
		}
		if (ei_decode_list_header(buffer, index, &arity) < 0) {
			(void) sdsfree(term);
			return (sds)NULL;
		}
		for (i = 0; i < arity; i++) {
			term = hierdis_call_decode_iolist(buffer, index, term);
			if (term == NULL) {
				break;
			}
		}
		return term;
	case ERL_NIL_EXT:
		if (ei_decode_list_header(buffer, index, &arity) < 0) {
			(void) sdsfree(term);
			return (sds)NULL;
		}
		return term;
	default:
		(void) sdsfree(term);
		return (sds)NULL;
	}
}

static void
hierdis_call_command_execute(hierdis_call_t *call, unsigned int hiredis_argc,
		const char* hiredis_argv[], size_t hiredis_argv_lengths[])
{
	char *buffer;
	int *index;
	sds term;
	unsigned int i;

	buffer = call->buf;
	index = &(call->idx);

	for (i = 0; i < hiredis_argc; i++) {
		term = sdsempty();
		term = hierdis_call_decode_iolist(buffer, index, term);
		if (term == NULL) {
			(void) hierdis_call_badarg(call);
			break;
		}
		hiredis_argv_lengths[i] = (size_t)sdslen(term);
		hiredis_argv[i] = (char *)term;
	}

	if (i < hiredis_argc) {
		(void) hierdis_call_command_free(call, i, hiredis_argv, hiredis_argv_lengths);
		(void) hierdis_call_badarg(call);
		return;
	}

	if (redisAsyncCommandArgv(call->port->context, hierdis_port_on_reply, (void *)NULL, hiredis_argc, hiredis_argv, hiredis_argv_lengths) != REDIS_OK) {
		(void) hierdis_call_command_free(call, hiredis_argc, hiredis_argv, hiredis_argv_lengths);
		(void) hierdis_call_badarg(call);
		return;
	}

	(void) hierdis_call_command_write(call);
	(void) hierdis_call_command_free(call, hiredis_argc, hiredis_argv, hiredis_argv_lengths);
}

static void
hierdis_call_command_free(hierdis_call_t *call, unsigned int hiredis_argc,
		const char* hiredis_argv[], size_t hiredis_argv_lengths[])
{
	unsigned int i;

	for (i = 0; i < hiredis_argc; i++) {
		if (hiredis_argv[i] != NULL) {
			(void) sdsfree((sds)hiredis_argv[i]);
			hiredis_argv[i] = NULL;
		}
	}
}

static void
hierdis_call_command_write(hierdis_call_t *call)
{
	int *length;
	char *buffer;
	int *index;
	EI_LONGLONG bufsize;

	length = &(call->olen);
	buffer = call->rbuf;
	index = &(call->ridx);

	bufsize = (EI_LONGLONG)(sdslen(call->port->context->c.obuf));

	ei_encode_version(NULL, length);
	ei_encode_tuple_header(NULL, length, 2);
	ei_encode_atom(NULL, length, HI_STRING(ok));
	ei_encode_longlong(NULL, length, bufsize);

	if ((int)call->rlen < *length) {
		buffer = (char *)driver_realloc((void *)buffer, (ErlDrvSizeT)*length);
		call->rlen = (ErlDrvSizeT)*length;
	}

	ei_encode_version(buffer, index);
	ei_encode_tuple_header(buffer, index, 2);
	ei_encode_atom(buffer, index, HI_STRING(ok));
	ei_encode_longlong(buffer, index, bufsize);
}

/*
 * Error functions
 */
static void
hierdis_call_badarg(hierdis_call_t *call)
{
	call->olen = (int)ERL_DRV_ERROR_BADARG;
}

static void
hierdis_call_error(hierdis_call_t *call, char *atom, char *reason)
{
	int *length;
	char *buffer;
	int *index;

	length = &(call->olen);
	buffer = call->rbuf;
	index = &(call->ridx);

	ei_encode_version(NULL, length);
	ei_encode_tuple_header(NULL, length, 2);
	ei_encode_atom(NULL, length, HI_STRING(error));
	ei_encode_tuple_header(NULL, length, 2);
	ei_encode_atom(NULL, length, atom);
	ei_encode_string(NULL, length, reason);

	if ((int)call->rlen < *length) {
		buffer = (char *)driver_realloc((void *)buffer, (ErlDrvSizeT)*length);
		call->rlen = (ErlDrvSizeT)*length;
	}

	ei_encode_version(buffer, index);
	ei_encode_tuple_header(buffer, index, 2);
	ei_encode_atom(buffer, index, HI_STRING(error));
	ei_encode_tuple_header(buffer, index, 2);
	ei_encode_atom(buffer, index, atom);
	ei_encode_string(buffer, index, reason);
}

static void
hierdis_call_redis_error(hierdis_call_t *call, int code, const char *reason)
{
	char *atom;
	sds atom_code;

	atom_code = NULL;

	switch(code) {
	case REDIS_REPLY_ERROR:
		atom = HI_STRING(redis_reply_error);
		break;
	case REDIS_ERR_IO:
		atom = HI_STRING(redis_err_io);
		break;
	case REDIS_ERR_EOF:
		atom = HI_STRING(redis_err_eof);
		break;
	case REDIS_ERR_PROTOCOL:
		atom = HI_STRING(redis_err_protocol);
		break;
	case REDIS_ERR_OOM:
		atom = HI_STRING(redis_err_oom);
		break;
	case REDIS_ERR_OTHER:
		atom = HI_STRING(redis_err_other);
		break;
	default:
		atom_code = sdscatprintf(sdsempty(), "redis_err_code_%d", code);
		if (atom_code == NULL) {
			(void) hierdis_call_badarg(call);
			return;
		}
		atom = (char *)atom_code;
		break;
	}

	(void) hierdis_call_error(call, atom, (char *)reason);
	if (atom_code != NULL) {
		(void) sdsfree(atom_code);
	}

	driver_failure_eof(call->port->drv_port);
}
