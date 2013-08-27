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

#define INIT_ATOM(NAME)		hierdis_drv->am_ ## NAME = driver_mk_atom(#NAME)

/*
 * Erlang DRV functions
 */
static int
hierdis_drv_init(void)
{
	hierdis_mutex = erl_drv_mutex_create("hierdis");

	if (hierdis_drv == NULL) {
		hierdis_drv = (hierdis_drv_term_data_t *)(driver_alloc(sizeof (hierdis_drv_term_data_t)));
		if (hierdis_drv == NULL) {
			return -1;
		}
	}

	INIT_ATOM(ok);
	INIT_ATOM(error);
	INIT_ATOM(normal);
	INIT_ATOM(undefined);

	/* Messages */
	INIT_ATOM(redis_closed);
	INIT_ATOM(redis_error);
	INIT_ATOM(redis_message);
	INIT_ATOM(redis_opened);
	INIT_ATOM(redis_reply);

	/* Errors */
	INIT_ATOM(redis_err_context);
	INIT_ATOM(redis_reply_error);
	INIT_ATOM(redis_err_io);
	INIT_ATOM(redis_err_eof);
	INIT_ATOM(redis_err_protocol);
	INIT_ATOM(redis_err_oom);
	INIT_ATOM(redis_err_other);
	INIT_ATOM(redis_err_timeout);

	return 0;
}

static void
hierdis_drv_finish(void)
{
	if (hierdis_drv == NULL) {
		return;
	}

	(void) driver_free(hierdis_drv);
	(void) erl_drv_mutex_destroy(hierdis_mutex);
}

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
	(void) hierdis_port_free((hierdis_port_t *)drv_data);
}

static void
hierdis_drv_ready_input(ErlDrvData drv_data, ErlDrvEvent drv_event)
{
	(void) drv_event; // Unused

	(void) hierdis_port_read((hierdis_port_t *)drv_data);
}

static void
hierdis_drv_ready_output(ErlDrvData drv_data, ErlDrvEvent drv_event)
{
	(void) drv_event; // Unused

	(void) hierdis_port_write((hierdis_port_t *)drv_data);
}

static void
hierdis_drv_timeout(ErlDrvData drv_data)
{
	(void) hierdis_port_timeout((hierdis_port_t *)drv_data);
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
}

static void
hierdis_drv_stop_select(ErlDrvEvent event, void *reserved)
{
	(void) event; // Unused
	(void) reserved; // Unused
}

DRIVER_INIT(hierdis_drv)
{
	return &hierdis_driver_entry;
}
