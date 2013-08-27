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

#ifndef HIERDIS_CALL_H
#define HIERDIS_CALL_H

#include "hierdis_drv_common.h"
#include "hiredis_erl_driver.h"

#define HIERDIS_CALL_CONNECT		0
#define HIERDIS_CALL_CONNECT_UNIX	1
#define HIERDIS_CALL_COMMAND		2
#define HIERDIS_CALL_DISCONNECT		3

hierdis_call_t	*hierdis_call_new(hierdis_port_t *port, unsigned int command, char *buf, ErlDrvSizeT len, char *rbuf, ErlDrvSizeT rlen);
void		hierdis_call_free(hierdis_call_t *call);
void		hierdis_call_execute(hierdis_call_t *call);

static void	hierdis_call_connect(hierdis_call_t *call);
static void	hierdis_call_connect_unix(hierdis_call_t *call);
static void	hierdis_call_command(hierdis_call_t *call);
static void	hierdis_call_disconnect(hierdis_call_t *call);

static void	hierdis_call_attach(hierdis_call_t *call, int with_timeout, unsigned long timeout);

static sds	hierdis_call_decode_iolist(char *buffer, int *index, sds term);
static void	hierdis_call_command_execute(hierdis_call_t *call, unsigned int hiredis_argc, const char* hiredis_argv[], size_t hiredis_argv_lengths[]);
static void	hierdis_call_command_free(hierdis_call_t *call, unsigned int hiredis_argc, const char* hiredis_argv[], size_t hiredis_argv_lengths[]);
static void	hierdis_call_command_write(hierdis_call_t *call);

/*
 * Error functions
 */
static void	hierdis_call_badarg(hierdis_call_t *call);
static void	hierdis_call_error(hierdis_call_t *call, char *atom, char *reason);
static void	hierdis_call_redis_error(hierdis_call_t *call, int code, const char *reason);

#endif
