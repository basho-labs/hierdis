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

#include "hierdis_drv.h"
#include <stdio.h>

// static void
// hierdis_drv_connect(hierdis_descriptor_t *desc, hierdis_call_t *call)
// {
// 	int arity;
// 	int type;
// 	int tlen;
// 	char *host;
// 	unsigned long port;
// 	unsigned long timeout;
// 	hierdis_error_t *error;

// 	if (ei_decode_tuple_header(call->buf, &(call->idx), &arity) < 0) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	if (arity < 2 || arity > 3) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	// Host
// 	ei_get_type(call->buf, &(call->idx), &type, &tlen);
// 	if (type != ERL_STRING_EXT) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}
// 	host = (char *)(driver_alloc(tlen + 1));
// 	if (host == NULL) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}
// 	if (ei_decode_string(call->buf, &(call->idx), host) < 0) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	// Port
// 	if (ei_decode_ulong(call->buf, &(call->idx), &port) < 0) {
// 		driver_free(host);
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	// Timeout
// 	if (arity == 3 && ei_decode_ulong(call->buf, &(call->idx), &timeout) < 0) {
// 		driver_free(host);
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	desc->context = redisAsyncConnect(host, port);
// 	driver_free(host);
// 	if (desc->context == NULL) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	} else if (desc->context->err) {
// 		error = hierdis_drv_create_redis_error(desc->context->err, desc->context->errstr);
// 		hierdis_drv_write_error(desc, call, error);
// 		return;
// 	}

// 	hierdis_drv_attach(desc, call, ((arity == 3) ? 1 : 0), timeout);
// }

// static void
// hierdis_drv_connect_unix(hierdis_descriptor_t *desc, hierdis_call_t *call)
// {
// 	int arity;
// 	int type;
// 	int tlen;
// 	char *path;
// 	unsigned long timeout;
// 	hierdis_error_t *error;

// 	if (ei_decode_tuple_header(call->buf, &(call->idx), &arity) < 0) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	if (arity < 1 || arity > 2) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	// Path
// 	ei_get_type(call->buf, &(call->idx), &type, &tlen);
// 	if (type != ERL_STRING_EXT) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}
// 	path = (char *)(driver_alloc(tlen + 1));
// 	if (path == NULL) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}
// 	if (ei_decode_string(call->buf, &(call->idx), path) < 0) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	// Timeout
// 	if (arity == 2 && ei_decode_ulong(call->buf, &(call->idx), &timeout) < 0) {
// 		driver_free(path);
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	desc->context = redisAsyncConnectUnix(path);
// 	driver_free(path);
// 	if (desc->context == NULL) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	} else if (desc->context->err) {
// 		error = hierdis_drv_create_redis_error(desc->context->err, desc->context->errstr);
// 		hierdis_drv_write_error(desc, call, error);
// 		return;
// 	}

// 	hierdis_drv_attach(desc, call, ((arity == 2) ? 1 : 0), timeout);
// }

// static void
// hierdis_drv_command(hierdis_descriptor_t *desc, hierdis_call_t *call)
// {
// 	int arity;
// 	erlang_ref ref;
// 	unsigned int argc;

// 	if (ei_decode_tuple_header(call->buf, &(call->idx), &arity) < 0) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	if (arity != 2) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	// Ref
// 	if (ei_decode_ref(call->buf, &(call->idx), &ref) < 0) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	// Args
// 	if (ei_decode_list_header(call->buf, &(call->idx), (int *)&argc) < 0) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	hierdis_drv_command_write(desc, call, &ref, argc);
// }

// static void
// hierdis_drv_command_write(hierdis_descriptor_t *desc, hierdis_call_t *call,
// 		erlang_ref *ref, unsigned int argc)
// {
// 	const char *argv[argc];
// 	size_t argv_lengths[argc];

// 	hierdis_drv_list_to_argv(call, argc, argv, argv_lengths);

// 	if (redisAsyncCommandArgv(desc->context, hierdis_drv_on_reply, (void *)ref, argc, argv, argv_lengths) != REDIS_OK) {
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	ei_encode_version(NULL, &(call->olen));
// 	ei_encode_tuple_header(NULL, &(call->olen), 2);
// 	ei_encode_atom(NULL, &(call->olen), "ok");
// 	ei_encode_ref(NULL, &(call->olen), ref);

// 	if (call->rlen < call->olen) {
// 		*(call->rbuf) = driver_alloc(call->olen);
// 		call->rlen = (ErlDrvSizeT)call->olen;
// 	}

// 	ei_encode_version(*(call->rbuf), &(call->ridx));
// 	ei_encode_tuple_header(*(call->rbuf), &(call->ridx), 2);
// 	ei_encode_atom(*(call->rbuf), &(call->ridx), "ok");
// 	ei_encode_ref(*(call->rbuf), &(call->ridx), ref);
// }

// static void
// hierdis_drv_list_to_argv(hierdis_call_t *call, unsigned int argc, const char* argv[], size_t argv_lengths[])
// {
// 	ETERM *iolist;
// 	ETERM *binary;
// 	int i;

// 	erl_init(NULL, 0);

// 	for (i = 0; i < argc; i++) {
// 		if (ei_decode_term(call->buf, &(call->idx), &iolist) < 0) {
// 			call->olen = (int)ERL_DRV_ERROR_BADARG;
// 			return;
// 		}
// 		if (iolist == NULL) {
// 			call->olen = (int)ERL_DRV_ERROR_BADARG;
// 			return;
// 		}
// 		binary = erl_iolist_to_binary(iolist);
// 		if (binary == NULL) {
// 			erl_free_term(iolist);
// 			call->olen = (int)ERL_DRV_ERROR_BADARG;
// 			return;
// 		}
// 		argv_lengths[i] = (size_t)ERL_BIN_SIZE(binary);
// 		argv[i] = (const char *)driver_alloc(argv_lengths[i]);
// 		memcpy((void *)argv[i], ERL_BIN_PTR(binary), ERL_BIN_SIZE(binary));
// 		erl_free_term(binary);
// 		erl_free_term(iolist);
// 	}
// }

// // static void list_to_hiredis_argv(ErlNifEnv* env, ERL_NIF_TERM list, unsigned int argc, const char* argv[], size_t argv_lengths[])
// // {
// //     ERL_NIF_TERM head, tail;
// //     ErlNifBinary list_elm;

// //     for(int i = 0; i < argc; i++)
// //     {
// //         enif_get_list_cell(env, list, &head, &tail);
// //         enif_inspect_iolist_as_binary(env, head, &list_elm);
        
// //         argv[i] = (const char*)list_elm.data;
// //         argv_lengths[i] = list_elm.size;

// //         list = tail;
// //     }
// // };

// static void
// hierdis_drv_disconnect(hierdis_descriptor_t *desc, hierdis_call_t *call)
// {
// 	ei_encode_version(NULL, &(call->olen));
// 	ei_encode_atom(NULL, &(call->olen), "true");

// 	if (call->rlen < call->olen) {
// 		*(call->rbuf) = driver_alloc(call->olen);
// 		call->rlen = (ErlDrvSizeT)call->olen;
// 	}

// 	ei_encode_version(*(call->rbuf), &(call->ridx));
// 	ei_encode_atom(*(call->rbuf), &(call->ridx), "true");
// }

// static hierdis_error_t *
// hierdis_drv_create_error(char *atom, char *reason)
// {
// 	hierdis_error_t *error;

// 	error = driver_alloc(sizeof (hierdis_error_t));
// 	if (error == NULL) {
// 		return (hierdis_error_t *)NULL;
// 	}
// 	error->atom = atom;
// 	error->afree = 0;
// 	error->reason = reason;
// 	error->rfree = 0;

// 	return error;
// }

// static hierdis_error_t *
// hierdis_drv_create_redis_error(int code, const char *reason)
// {

// 	hierdis_error_t *error;
// 	char *atom;
// 	int afree = 0;

// 	switch(code) {
// 	case REDIS_REPLY_ERROR:
// 		atom = "redis_reply_error";
// 		break;
// 	case REDIS_ERR_IO:
// 		atom = "redis_err_io";
// 		break;
// 	case REDIS_ERR_EOF:
// 		atom = "redis_err_eof";
// 		break;
// 	case REDIS_ERR_PROTOCOL:
// 		atom = "redis_err_protocol";
// 		break;
// 	case REDIS_ERR_OOM:
// 		atom = "redis_err_oom";
// 		break;
// 	case REDIS_ERR_OTHER:
// 		atom = "redis_err_other";
// 		break;
// 	default:
// 		(void) asprintf(&atom, "redis_err_code_%d", code);
// 		afree = 1;
// 		break;
// 	}

// 	error = hierdis_drv_create_error(atom, (char *)reason);
// 	if (error == NULL) {
// 		if (afree == 1) {
// 			driver_free(atom);
// 		}
// 		return error;
// 	}
// 	error->afree = afree;

// 	return error;
// }

// static void
// hierdis_drv_free_error(hierdis_error_t *error)
// {
// 	if (error == NULL) {
// 		return;
// 	}
// 	if (error->afree == 1) {
// 		driver_free(error->atom);
// 	}
// 	if (error->rfree == 1) {
// 		driver_free(error->reason);
// 	}
// 	driver_free(error);
// 	error = NULL;
// }

// static void
// hierdis_drv_write_error(hierdis_descriptor_t *desc, hierdis_call_t *call, hierdis_error_t *error)
// {
// 	if (error == NULL) {
// 		return;
// 	}

// 	ei_encode_version(NULL, &(call->olen));
// 	ei_encode_tuple_header(NULL, &(call->olen), 2);
// 	ei_encode_atom(NULL, &(call->olen), "error");
// 	ei_encode_tuple_header(NULL, &(call->olen), 2);
// 	ei_encode_atom(NULL, &(call->olen), error->atom);
// 	ei_encode_string(NULL, &(call->olen), error->reason);

// 	if (call->rlen < call->olen) {
// 		*(call->rbuf) = driver_alloc(call->olen);
// 		call->rlen = (ErlDrvSizeT) call->olen;
// 	}

// 	ei_encode_version(*(call->rbuf), &(call->ridx));
// 	ei_encode_tuple_header(*(call->rbuf), &(call->ridx), 2);
// 	ei_encode_atom(*(call->rbuf), &(call->ridx), "error");
// 	ei_encode_tuple_header(*(call->rbuf), &(call->ridx), 2);
// 	ei_encode_atom(*(call->rbuf), &(call->ridx), error->atom);
// 	ei_encode_string(*(call->rbuf), &(call->ridx), error->reason);

// 	hierdis_drv_free_error(error);
// }

// static void
// hierdis_drv_attach(hierdis_descriptor_t *desc, hierdis_call_t *call,
// 		int with_timeout, unsigned long timeout)
// {
// 	if (redisErlDriverAttach(desc->context, desc->erl_port, (ErlDrvData)desc) != REDIS_OK) {
// 		redisAsyncDisconnect(desc->context);
// 		desc->context = NULL;
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	if (redisAsyncSetConnectCallback(desc->context, hierdis_drv_on_connect) != REDIS_OK) {
// 		redisAsyncDisconnect(desc->context);
// 		desc->context = NULL;
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	if (redisAsyncSetDisconnectCallback(desc->context, hierdis_drv_on_disconnect) != REDIS_OK) {
// 		redisAsyncDisconnect(desc->context);
// 		desc->context = NULL;
// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
// 		return;
// 	}

// 	ei_encode_version(NULL, &(call->olen));
// 	ei_encode_atom(NULL, &(call->olen), "ok");

// 	if (call->rlen < call->olen) {
// 		*(call->rbuf) = driver_alloc(call->olen);
// 		call->rlen = (ErlDrvSizeT)call->olen;
// 	}

// 	ei_encode_version(*(call->rbuf), &(call->ridx));
// 	ei_encode_atom(*(call->rbuf), &(call->ridx), "ok");

// 	if (with_timeout == 1) {
// 		fprintf(stdout, "HAS TIMEOUT: %d\n", (int)timeout);
// 	}
// }

// static void
// hierdis_drv_on_connect(const struct redisAsyncContext *context, int status)
// {
// 	redisErlDriverEvents *p;
// 	hierdis_descriptor_t *desc;

// 	p = (redisErlDriverEvents *)context->ev.data;
// 	if (p == NULL) {
// 		return;
// 	}
// 	desc = (hierdis_descriptor_t *)p->drv_data;
// 	if (desc == NULL) {
// 		return;
// 	}
// 	desc->status = status;
// 	if (context->err) {
// 		TRACE_F(("ERROR: %d %s\n", context->err, context->errstr));
// 	}
// 	if (desc->status == REDIS_OK) {
// 		desc->connected = 1;
// 	} else {
// 		desc->connected = 0;
// 	}
// 	TRACE_F(("Connected: %d\n", desc->connected));
// }

// static void
// hierdis_drv_on_disconnect(const struct redisAsyncContext *context, int status)
// {
// 	redisErlDriverEvents *p;
// 	hierdis_descriptor_t *desc;

// 	TRACE_F(("TRYING TO DISCONNECT\n"));

// 	p = (redisErlDriverEvents *)context->ev.data;
// 	if (p == NULL) {
// 		return;
// 	}
// 	desc = (hierdis_descriptor_t *)p->drv_data;
// 	if (desc == NULL) {
// 		return;
// 	}
// 	desc->status = status;
// 	desc->connected = 0;
// 	TRACE_F(("Disconnected: %d %d\n", desc->connected, desc->status));
// }

// static ERL_NIF_TERM hierdis_make_binary_from_reply(ErlNifEnv* env, redisReply* r)
// {
//     ERL_NIF_TERM term;
    
//     hiredis_reply_handle* handle = (hiredis_reply_handle*)enif_alloc_resource(HIREDIS_REPLY_RESOURCE, sizeof(hiredis_reply_handle)); 
//     handle->reply = r;
//     term = enif_make_resource_binary(env, handle, handle->reply->str, handle->reply->len);    
//     enif_release_resource(handle);

//     return term;
// }
// static ErlDrvTermData *
// hierdis_drv_reply_to_binary_spec(hierdis_descriptor_t *desc, redisReply *reply)
// {

// }

// static ErlDrvTermData *
// hierdis_drv_reply_to_spec(hierdis_descriptor_t *desc, redisReply *reply)
// {
// 	ErlDrvTermData spec[];

// 	switch (reply->type) {
// 	case REDIS_REPLY_STRING:
// 	case REDIS_REPLY_STATUS:
// 		spec = hierdis_drv_reply_to_binary_spec(desc, reply);
// 		break;
// 	case REDIS_REPLY_ARRAY:
// 		spec = hierdis_drv_reply_to_list_spec(desc, reply);
// 		break;
// 	case REDIS_REPLY_INTEGER:
// 		{ERL_DRV_INT64, reply->integer};
// 		break;
// 	case REDIS_REPLY_NIL:
// 		spec = {ERL_DRV_ATOM, desc->term_undefined};
// 		hierdis_drv_free_reply(reply);
// 		break;
// 	default:
// 		spec = {
// 			ERL_DRV_ATOM, desc->term_error,
// 				ERL_DRV_ATOM, driver_mk_atom("redis_reply_error"),
// 				ERL_DRV_ATOM, driver_mk_atom("Unknown reply error."),
// 				ERL_DRV_TUPLE, 2,
// 			ERL_DRV_TUPLE, 2
// 		};
// 		hierdis_drv_free_reply(reply);
// 		break;
// 	}
// 	return spec;
// 	switch(r->type)
//     {
//         case REDIS_REPLY_STRING:
//         case REDIS_REPLY_STATUS:
//             term = hierdis_make_binary_from_reply(env, r);
//             break;
//         case REDIS_REPLY_ARRAY:
//             term = hierdis_make_list_from_reply(env, r);
//             hierdis_free_reply(r);
//             break;
//         case REDIS_REPLY_INTEGER:
//             term = enif_make_int64(env, r->integer);
//             hierdis_free_reply(r);
//             break;
//         case REDIS_REPLY_NIL:
//             term = enif_make_atom(env, "undefined\0");
//             hierdis_free_reply(r);
//             break;
//         default:
//             term = hierdis_make_error(env, REDIS_REPLY_ERROR, "Unknown reply error.");
//             hierdis_free_reply(r);
//     }
// }

// static void
// hierdis_drv_on_reply(struct redisAsyncContext *context, void *r, void *privdata)
// {
// 	redisReply *reply;
// 	redisErlDriverEvents *p;
// 	hierdis_descriptor_t *desc;
// 	const erlang_ref *ref;
// 	// char *ref_buf;
// 	// int ref_len;
// 	// int ref_idx;

// 	reply = (redisReply *)r;
// 	if (reply == NULL) {
// 		fprintf(stdout, "NULL REPLY\n");
// 		return;
// 	}
// 	p = (redisErlDriverEvents *)context->ev.data;
// 	if (p == NULL) {
// 		fprintf(stdout, "NULL events\n");
// 		return;
// 	}
// 	desc = (hierdis_descriptor_t *)p->drv_data;
// 	if (desc == NULL) {
// 		fprintf(stdout, "NULL desc\n");
// 		return;
// 	}
// 	ref = (const erlang_ref *)privdata;
// 	if (ref == NULL) {
// 		TRACE_F(("BAD REF\n"));
// 		return;
// 	}

// 	// TRACE_F(("ABOUT TO ENCODE REF\n"));

// 	// ref_len = 0;
// 	// ref_idx = 0;

// 	// ei_encode_version(NULL, &ref_len);
// 	// ei_encode_ref(NULL, &ref_len, ref);
// 	// ref_buf = (char *)driver_alloc(ref_len);
// 	// ei_encode_version(ref_buf, &ref_idx);
// 	// ei_encode_ref(ref_buf, &ref_idx, ref);

// 	// TRACE_F(("REF: %d\n", ref_len));

// 	ErlDrvTermData spec[] = {
// 		ERL_DRV_ATOM, desc->term_reply,
// 		ERL_DRV_PORT, desc->term_port,
// 			// ERL_DRV_EXT2TERM, (ErlDrvTermData)ref_buf, ref_len,
// 			ERL_DRV_BUF2BINARY, (ErlDrvTermData)reply->str, reply->len,
// 			// ERL_DRV_TUPLE, 2,
// 		ERL_DRV_TUPLE, 3
// 	};
// 	erl_drv_output_term(driver_mk_port(desc->erl_port), spec, sizeof(spec) / sizeof(spec[0]));

// 	// driver_free(ref_buf);
// }

/*
 * PORT
 */
static hierdis_port_t *
hierdis_port_new(ErlDrvPort erl_port)
{
	hierdis_port_t *port;

	port = (hierdis_port_t *)(driver_alloc(sizeof (hierdis_port_t)));
	if (port == NULL) {
		return port;
	}

	port->context = (redisAsyncContext *)NULL;
	port->erl_port = erl_port;
	port->term_port = driver_mk_port(erl_port);
	port->term_error = driver_mk_atom("error");
	port->term_reply = driver_mk_atom("reply");
	port->term_undefined = driver_mk_atom("undefined");
	port->connected = 0;
	port->status = 0;

	return port;
}

static void
hierdis_port_free(hierdis_port_t *port)
{
	if (port == NULL) {
		return;
	}
	(void) driver_free(port);
}

static void
hierdis_port_read(hierdis_port_t *port)
{
	(void) redisAsyncHandleRead(port->context);
}

static void
hierdis_port_write(hierdis_port_t *port)
{
	(void) redisAsyncHandleWrite(port->context);
}

/*
 * CALL
 */
static hierdis_call_t *
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
			(void) hierdis_call_make_error(call, "no_hiredis_context", 0, "connect/2, connect/3, connect_unix/1, connect_unix/2", 0);
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
	(void) hierdis_call_badarg(call);
}

static void
hierdis_call_connect_unix(hierdis_call_t *call)
{
	(void) hierdis_call_badarg(call);
}

/*
 * CALL: Write Error functions
 */
static void
hierdis_call_badarg(hierdis_call_t *call)
{
	call->olen = (int)ERL_DRV_ERROR_BADARG;
}

static void
hierdis_call_make_error(hierdis_call_t *call, char *atom, int atom_needs_free, char *reason, int reason_needs_free)
{
	hierdis_call_error_t *error;

	error = hierdis_call_error_new(call, atom, atom_needs_free, reason, reason_needs_free);
	if (error == NULL) {
		(void) hierdis_call_badarg(call);
		return;
	}

	(void) hierdis_call_error_write(error);
	(void) hierdis_call_error_free(error);
}


/*
 * CALL: Error functions
 */
static hierdis_call_error_t *
hierdis_call_error_new(hierdis_call_t *call, char *atom, int atom_needs_free, char *reason, int reason_needs_free)
{
	hierdis_call_error_t *error;

	if (call == NULL) {
		return (hierdis_call_error_t *)NULL;
	}

	error = driver_alloc(sizeof (hierdis_call_error_t));
	if (error == NULL) {
		return error;
	}

	error->call = call;
	error->atom = atom;
	error->atom_needs_free = atom_needs_free;
	error->reason = reason;
	error->reason_needs_free = reason_needs_free;

	return error;
}

static void
hierdis_call_error_free(hierdis_call_error_t *error)
{
	if (error == NULL) {
		return;
	}
	if (error->atom_needs_free == 1) {
		(void) driver_free(error->atom);
	}
	if (error->reason_needs_free == 1) {
		(void) driver_free(error->reason);
	}
	(void) driver_free(error);
}

static void
hierdis_call_error_write(hierdis_call_error_t *error)
{
	int *length;
	char *buffer;
	int *index;

	if (error->call == NULL) {
		return;
	}

	length = &(error->call->olen);
	buffer = error->call->rbuf;
	index = &(error->call->ridx);

	ei_encode_version(NULL, length);
	ei_encode_tuple_header(NULL, length, 2);
	ei_encode_atom(NULL, length, "error");
	ei_encode_tuple_header(NULL, length, 2);
	ei_encode_atom(NULL, length, error->atom);
	ei_encode_string(NULL, length, error->reason);

	if ((int)error->call->rlen < *length) {
		buffer = (char *)driver_realloc((void *)buffer, (ErlDrvSizeT)*length);
		error->call->rlen = (ErlDrvSizeT)*length;
	}

	ei_encode_version(buffer, index);
	ei_encode_tuple_header(buffer, index, 2);
	ei_encode_atom(buffer, index, "error");
	ei_encode_tuple_header(buffer, index, 2);
	ei_encode_atom(buffer, index, error->atom);
	ei_encode_string(buffer, index, error->reason);
}

/*
 * Erlang DRV functions
 */
static ErlDrvData
hierdis_drv_start(ErlDrvPort port, char *command)
{
	hierdis_port_t *desc;

	(void) command; // Unused

	desc = hierdis_port_new(port);
	if (desc == NULL) {
		return ERL_DRV_ERROR_GENERAL;
	}

	return (ErlDrvData)desc;
}

static void
hierdis_drv_stop(ErlDrvData drv_data)
{
	hierdis_port_free((hierdis_port_t *)drv_data);
}

static void
hierdis_drv_ready_input(ErlDrvData drv_data, ErlDrvEvent drv_event)
{
	(void) drv_event; // Unused

	hierdis_port_read((hierdis_port_t *)drv_data);
}

static void
hierdis_drv_ready_output(ErlDrvData drv_data, ErlDrvEvent drv_event)
{
	(void) drv_event; // Unused

	hierdis_port_write((hierdis_port_t *)drv_data);
}

static ErlDrvSSizeT
hierdis_drv_call(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len,
		char **rbuf, ErlDrvSizeT rlen, unsigned int *flags)
{
	hierdis_call_t *call;
	ErlDrvSSizeT olen;

	(void) flags; // Unused

	call = hierdis_call_new((hierdis_port_t *)drv_data, command, buf, len, *rbuf, rlen);
	if (call == NULL) {
		return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
	}

	(void) hierdis_call_execute(call);
	olen = (ErlDrvSSizeT)call->olen;
	(void) hierdis_call_free(call);

	return olen;

	// hierdis_descriptor_t *desc;

	// hierdis_call_t *call;
	// hierdis_error_t *error;
	// int version;
	// ErlDrvSSizeT olen;

	// desc = (hierdis_descriptor_t *)drv_data;
	// call = driver_alloc(sizeof (hierdis_call_t));
	// if (call == NULL) {
	// 	return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
	// }
	// call->command = command;
	// call->buf = buf;
	// call->len = len;
	// call->idx = 0;
	// call->rbuf = rbuf;
	// call->rlen = rlen;
	// call->ridx = 0;
	// call->olen = 0;

	// if (ei_decode_version(call->buf, &(call->idx), &version) < 0) {
	// 	driver_free(call);
	// 	return (ErlDrvSSizeT)ERL_DRV_ERROR_BADARG;
	// }

	// if (desc->context == NULL) {
	// 	switch (call->command) {
	// 	case HIERDIS_DRV_CONNECT:
	// 		hierdis_drv_connect(desc, call);
	// 		break;
	// 	case HIERDIS_DRV_CONNECT_UNIX:
	// 		hierdis_drv_connect_unix(desc, call);
	// 		break;
	// 	case HIERDIS_DRV_COMMAND:
	// 	case HIERDIS_DRV_DISCONNECT:
	// 		error = hierdis_drv_create_error("no_hiredis_context", "connect/2, connect/3, connect_unix/1, connect_unix/2");
	// 		hierdis_drv_write_error(desc, call, error);
	// 		break;
	// 	default:
	// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
	// 		break;
	// 	}
	// } else {
	// 	switch (command) {
	// 	case HIERDIS_DRV_CONNECT:
	// 	case HIERDIS_DRV_CONNECT_UNIX:
	// 		error = hierdis_drv_create_error("existing_hiredis_context", "command/1, disconnect/1");
	// 		hierdis_drv_write_error(desc, call, error);
	// 		break;
	// 	case HIERDIS_DRV_COMMAND:
	// 		hierdis_drv_command(desc, call);
	// 		break;
	// 	case HIERDIS_DRV_DISCONNECT:
	// 		hierdis_drv_disconnect(desc, call);
	// 		break;
	// 	default:
	// 		call->olen = (int)ERL_DRV_ERROR_BADARG;
	// 		break;
	// 	}
	// }

	// olen = (ErlDrvSSizeT)call->olen;
	// driver_free(call);
	// return olen;
}

static void
hierdis_drv_stop_select(ErlDrvEvent event, void *reserved)
{
	fprintf(stdout, "STOP SELECT: %d\n", (int)event);
}

DRIVER_INIT(hierdis_drv)
{
	return &hierdis_driver_entry;
}
