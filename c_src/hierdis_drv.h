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

#include <erl_driver.h>
#include <erl_interface.h>
#include <string.h>
#include "hiredis_erl_driver.h"
#include "sds.h"

#define TRACE	1
#ifdef TRACE
	#define TRACE_C(c)	do { putchar(c); fflush(stdout); } while (0)
	#define TRACE_S(s)	do { fputs((s), stdout); fflush(stdout); } while (0)
	#define TRACE_F(args)	do { printf args ;fflush(stdout); } while (0)
#else
	#define TRACE_C(c)	((void)(0))
	#define TRACE_S(s)	((void)(0))
	#define TRACE_F(args)	((void)(0))
#endif

/*
 * PORT
 */
typedef struct hierdis_port {
	redisAsyncContext	*context;
	ErlDrvPort		erl_port;
	ErlDrvTermData		term_port;
	ErlDrvTermData		term_error;
	ErlDrvTermData		term_reply;
	ErlDrvTermData		term_undefined;
	int			connected;
	int			status;
} hierdis_port_t;

static hierdis_port_t	*hierdis_port_new(ErlDrvPort erl_port);
static void		hierdis_port_free(hierdis_port_t *port);
static void		hierdis_port_read(hierdis_port_t *port);
static void		hierdis_port_write(hierdis_port_t *port);

/*
 * CALL
 */
#define HIERDIS_CALL_CONNECT		0
#define HIERDIS_CALL_CONNECT_UNIX	1
#define HIERDIS_CALL_COMMAND		2
#define HIERDIS_CALL_DISCONNECT		3

typedef struct hierdis_call {
	hierdis_port_t	*port;
	unsigned int	cmd;
	char		*buf;
	ErlDrvSizeT	len;
	int		idx;
	char		*rbuf;
	ErlDrvSizeT	rlen;
	int		ridx;
	int		olen;
} hierdis_call_t;

typedef struct hierdis_call_error {
	hierdis_call_t	*call;
	char		*atom;
	int		atom_needs_free;
	char		*reason;
	int		reason_needs_free;
} hierdis_call_error_t;

static hierdis_call_t	*hierdis_call_new(hierdis_port_t *port, unsigned int command, char *buf, ErlDrvSizeT len, char *rbuf, ErlDrvSizeT rlen);
static void		hierdis_call_free(hierdis_call_t *call);

static void		hierdis_call_execute(hierdis_call_t *call);
static void		hierdis_call_connect(hierdis_call_t *call);
static void		hierdis_call_connect_unix(hierdis_call_t *call);

/*
 * CALL: Write Error functions
 */
static void		hierdis_call_badarg(hierdis_call_t *call);
static void		hierdis_call_make_error(hierdis_call_t *call, char *atom, int atom_needs_free, char *reason, int reason_needs_free);

/*
 * CALL: Error functions
 */
static hierdis_call_error_t	*hierdis_call_error_new(hierdis_call_t *call, char *atom, int atom_needs_free, char *reason, int reason_needs_free);
static void			hierdis_call_error_free(hierdis_call_error_t *error);
static void			hierdis_call_error_write(hierdis_call_error_t *error);

// typedef struct hierdis_call {
// 	unsigned int	command;
// 	char		*buf;
// 	ErlDrvSizeT	len;
// 	int		idx;
// 	char		**rbuf;
// 	ErlDrvSizeT	rlen;
// 	int		ridx;
// 	int		olen;
// } hierdis_call_t;

// typedef struct hierdis_descriptor {
// 	redisAsyncContext	*context;
// 	ErlDrvPort		erl_port;
// 	ErlDrvTermData		term_port;
// 	ErlDrvTermData		term_error;
// 	ErlDrvTermData		term_reply;
// 	ErlDrvTermData		term_undefined;
// 	int			connected;
// 	int			status;
// } hierdis_descriptor_t;

// typedef struct hierdis_error {
// 	char		*atom;
// 	int		afree;
// 	char		*reason;
// 	int		rfree;
// } hierdis_error_t;

// typedef struct hierdis_response {
// 	char		*buf;
// 	ErlDrvSizeT	len;
// 	int		idx;
// } hierdis_response_t;

// static void	hierdis_drv_connect(hierdis_descriptor_t *, hierdis_call_t *);
// static void	hierdis_drv_connect_unix(hierdis_descriptor_t *, hierdis_call_t *);
// static void	hierdis_drv_command(hierdis_descriptor_t *, hierdis_call_t *);
// static void	hierdis_drv_disconnect(hierdis_descriptor_t *, hierdis_call_t *);

// static void	hierdis_drv_command_write(hierdis_descriptor_t *, hierdis_call_t *, erlang_ref *, unsigned int);
// static void	hierdis_drv_list_to_argv(hierdis_call_t *, unsigned int, const char*[], size_t[]);

// static hierdis_error_t	*hierdis_drv_create_error(char *, char *);
// static hierdis_error_t	*hierdis_drv_create_redis_error(int, const char *);
// static void		hierdis_drv_free_error(hierdis_error_t *);
// static void		hierdis_drv_write_error(hierdis_descriptor_t *, hierdis_call_t *, hierdis_error_t *);

// static void	hierdis_drv_attach(hierdis_descriptor_t *, hierdis_call_t *, int, unsigned long);
// static void	hierdis_drv_on_connect(const struct redisAsyncContext *, int);
// static void	hierdis_drv_on_disconnect(const struct redisAsyncContext *, int);
// static void	hierdis_drv_on_reply(struct redisAsyncContext *, void *, void *);

/*
 * Erlang DRV functions
 */
static ErlDrvData	hierdis_drv_start(ErlDrvPort, char *);
static void		hierdis_drv_stop(ErlDrvData);
static void		hierdis_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void		hierdis_drv_ready_output(ErlDrvData, ErlDrvEvent);
static ErlDrvSSizeT	hierdis_drv_call(ErlDrvData, unsigned int, char *, ErlDrvSizeT, char **, ErlDrvSizeT, unsigned int *);
static void		hierdis_drv_stop_select(ErlDrvEvent, void *);

static ErlDrvEntry	hierdis_driver_entry = {
	NULL,				/* F_PTR init, called when driver is loaded */
	hierdis_drv_start,		/* L_PTR start, called when port is opened */
	hierdis_drv_stop,		/* F_PTR stop, called when port is closed */
	NULL,				/* F_PTR output, called when erlang has sent */
	hierdis_drv_ready_input,	/* F_PTR ready_input, called when input descriptor ready */
	hierdis_drv_ready_output,	/* F_PTR ready_output, called when output descriptor ready */
	"hierdis_drv",			/* char *driver_name, the argument to open_port */
	NULL,				/* F_PTR finish, called when unloaded */
	NULL,				/* void *handle, Reserved by VM */
	NULL,				/* F_PTR control, port_command callback */
	NULL,				/* F_PTR timeout, reserved */
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
