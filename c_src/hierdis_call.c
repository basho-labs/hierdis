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

void
hierdis_call_connect(hierdis_call_t *call)
{
	(void) hierdis_call_badarg(call);
}

void
hierdis_call_connect_unix(hierdis_call_t *call)
{
	(void) hierdis_call_badarg(call);
}

/*
 * Error functions
 */
void
hierdis_call_badarg(hierdis_call_t *call)
{
	call->olen = (int)ERL_DRV_ERROR_BADARG;
}

void
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
 * Call Error functions
 */
hierdis_call_error_t *
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

void
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

void
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
