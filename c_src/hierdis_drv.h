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

#ifndef HIERDIS_DRV_H
#define HIERDIS_DRV_H

#include "hierdis_drv_common.h"
#include "hiredis_erl_driver.h"

hierdis_drv_term_data_t	*hierdis_drv;
ErlDrvMutex		*hierdis_mutex;

/*
 * Erlang DRV functions
 */
static int		hierdis_drv_init(void);
static void		hierdis_drv_finish(void);
static ErlDrvData	hierdis_drv_start(ErlDrvPort drv_port, char *command);
static void		hierdis_drv_stop(ErlDrvData drv_data);
static void		hierdis_drv_ready_input(ErlDrvData drv_data, ErlDrvEvent drv_event);
static void		hierdis_drv_ready_output(ErlDrvData drv_data, ErlDrvEvent drv_event);
static void		hierdis_drv_timeout(ErlDrvData drv_data);
static ErlDrvSSizeT	hierdis_drv_call(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen, unsigned int *flags);
static void		hierdis_drv_stop_select(ErlDrvEvent drv_event, void *reserved);

static ErlDrvEntry	hierdis_driver_entry = {
	hierdis_drv_init,		/* F_PTR init, called when driver is loaded */
	hierdis_drv_start,		/* L_PTR start, called when port is opened */
	hierdis_drv_stop,		/* F_PTR stop, called when port is closed */
	NULL,				/* F_PTR output, called when erlang has sent */
	hierdis_drv_ready_input,	/* F_PTR ready_input, called when input descriptor ready */
	hierdis_drv_ready_output,	/* F_PTR ready_output, called when output descriptor ready */
	"hierdis_drv",			/* char *driver_name, the argument to open_port */
	hierdis_drv_finish,		/* F_PTR finish, called when unloaded */
	NULL,				/* void *handle, Reserved by VM */
	NULL,				/* F_PTR control, port_command callback */
	hierdis_drv_timeout,		/* F_PTR timeout, reserved */
	NULL,				/* F_PTR outputv, reserved */
	NULL,				/* F_PTR ready_async, only for async drivers */
	NULL,				/* F_PTR flush, called when port is about to be closed, but there is data in driver queue */
	hierdis_drv_call,		/* F_PTR call, much like control, sync call to driver */
	NULL,				/* F_PTR event, called when an event selected by driver_event() occurs. */
	ERL_DRV_EXTENDED_MARKER,	/* int extended marker, Should always be set to indicate driver versioning */
	ERL_DRV_EXTENDED_MAJOR_VERSION,	/* int major_version, should always be set to this value */
	ERL_DRV_EXTENDED_MINOR_VERSION,	/* int minor_version, should always be set to this value */
	ERL_DRV_FLAG_USE_PORT_LOCKING,	/* int driver_flags, see documentation */
	NULL,				/* void *handle2, reserved for VM use */
	NULL,				/* F_PTR process_exit, called when a monitored process dies */
	hierdis_drv_stop_select		/* F_PTR stop_select, called to close an event object */
};

#endif
