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

#ifndef HIERDIS_DRV_COMMON_H
#define HIERDIS_DRV_COMMON_H

#include <erl_driver.h>
#include <erl_interface.h>
#include <unistd.h>
#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include "hiredis.h"
#include "async.h"
#include "sds.h"

// #define TRACE	1
#ifdef TRACE
	#define TRACE_C(c)	do { putchar(c); fflush(stdout); } while (0)
	#define TRACE_S(s)	do { fputs((s), stdout); fflush(stdout); } while (0)
	#define TRACE_F(args)	do { printf args ;fflush(stdout); } while (0)
#else
	#define TRACE_C(c)	((void)(0))
	#define TRACE_S(s)	((void)(0))
	#define TRACE_F(args)	((void)(0))
#endif

/* hierdis_port.h */
typedef struct hierdis_port {
	redisAsyncContext	*context;
	ErlDrvPort		drv_port;
	ErlDrvTermData		term_port;
	sds			err_atom;
	sds			err_reason;
	unsigned long		timeout;
	int			opened;
	int			flags;
	int			fixflags;
} hierdis_port_t;

extern hierdis_port_t	*hierdis_port_new(ErlDrvPort drv_port);
extern void		hierdis_port_free(hierdis_port_t *port);
extern void		hierdis_port_stop(hierdis_port_t *port);
extern void		hierdis_port_read(hierdis_port_t *port);
extern void		hierdis_port_write(hierdis_port_t *port);
extern void		hierdis_port_timeout(hierdis_port_t *port);
extern void		hierdis_port_on_connect(const struct redisAsyncContext *context, int status);
extern void		hierdis_port_on_disconnect(const struct redisAsyncContext *context, int status);
extern void		hierdis_port_on_reply(struct redisAsyncContext *context, void *replydata, void *privdata);

/* hierdis_call.h */
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

extern hierdis_call_t	*hierdis_call_new(hierdis_port_t *port, unsigned int command, char *buf, ErlDrvSizeT len, char *rbuf, ErlDrvSizeT rlen);
extern void		hierdis_call_free(hierdis_call_t *call);
extern void		hierdis_call_execute(hierdis_call_t *call);

/* common */
typedef struct hierdis_drv_term_data {
	ErlDrvTermData		am_ok;
	ErlDrvTermData		am_error;
	ErlDrvTermData		am_normal;
	ErlDrvTermData		am_undefined;

	/* Message Types */
	ErlDrvTermData		am_redis_closed;
	ErlDrvTermData		am_redis_error;
	ErlDrvTermData		am_redis_message;
	ErlDrvTermData		am_redis_opened;
	ErlDrvTermData		am_redis_reply;

	/* Errors */
	ErlDrvTermData		am_redis_err_context;
	ErlDrvTermData		am_redis_reply_error;
	ErlDrvTermData		am_redis_err_io;
	ErlDrvTermData		am_redis_err_eof;
	ErlDrvTermData		am_redis_err_protocol;
	ErlDrvTermData		am_redis_err_oom;
	ErlDrvTermData		am_redis_err_other;
	ErlDrvTermData		am_redis_err_timeout;

	sds		str_ok;
	sds		str_error;
	sds		str_redis_err_context;
	sds		str_redis_reply_error;
	sds		str_redis_err_io;
	sds		str_redis_err_eof;
	sds		str_redis_err_protocol;
	sds		str_redis_err_oom;
	sds		str_redis_err_other;
	sds		str_redis_err_timeout;
} hierdis_drv_term_data_t;

extern hierdis_drv_term_data_t	*hierdis_drv;
extern ErlDrvMutex		*hierdis_mutex;

#define HI_ATOM(NAME)		(ErlDrvTermData)(hierdis_drv->am_ ## NAME)
#define HI_STRING(NAME)		(char *)(hierdis_drv->str_ ## NAME)

#define HI_FAIL_BADSPEC(PORT)		(void)(driver_failure_atom(PORT, "bad_spec"))
#define HI_FAIL_OOM(PORT)		(void)(driver_failure_atom(PORT, "out_of_memory"))

#endif
