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

#ifndef HIERDIS_PORT_H
#define HIERDIS_PORT_H

#include "hierdis_drv_common.h"
#include "hiredis_erl_driver.h"

#define PORT_BLOCK_SIZE	4

typedef struct hierdis_port_spec {
	ErlDrvSizeT		size;
	ErlDrvTermData		*data;
	int			index;
	hierdis_port_t		*port;
	redisAsyncContext	*context;
} hierdis_port_spec_t;

hierdis_port_t	*hierdis_port_new(ErlDrvPort drv_port);
void		hierdis_port_free(hierdis_port_t *port);
void		hierdis_port_read(hierdis_port_t *port);
void		hierdis_port_write(hierdis_port_t *port);
void		hierdis_port_timeout(hierdis_port_t *port);
void		hierdis_port_on_connect(const struct redisAsyncContext *context, int status);
void		hierdis_port_on_disconnect(const struct redisAsyncContext *context, int status);
void		hierdis_port_on_reply(struct redisAsyncContext *context, void *replydata, void *privdata);

static hierdis_port_spec_t *	spec_new(struct redisAsyncContext *context, hierdis_port_t *port);
static void			spec_free(hierdis_port_spec_t *spec);
static int			fix_spec(hierdis_port_spec_t *spec, ErlDrvSizeT new_size);

// static int	hierdis_port_response_head(hierdis_port_t *port, redisReply *reply, ErlDrvTermData *spec, int index);
// static int	hierdis_port_response_head(redisReply *reply, hierdis_port_spec_t *spec);
// static int	hierdis_port_response_tail(redisReply *reply, hierdis_port_spec_t *spec);
// static int	hierdis_port_response_tail(hierdis_port_t *port, redisReply *reply, ErlDrvTermData *spec, int index);
// static int	hierdis_port_make_response(redisReply *reply, hierdis_port_spec_t *spec);

static ErlDrvTermData	hierdis_port_get_redis_error(int code);
static sds		hierdis_port_get_redis_error_string(int code);

static void		hierdis_port_closed(hierdis_port_t *port);
static void		hierdis_port_close_error(hierdis_port_t *port);
static void		hierdis_port_opened(hierdis_port_t *port);
static void		hierdis_port_open_error(hierdis_port_t *port);
static void		hierdis_port_open_timeout(hierdis_port_t *port);
static void		hierdis_port_respond(hierdis_port_t *port, redisReply *reply, int subscribed);
static void		hierdis_port_output(hierdis_port_spec_t *spec);

static int	hierdis_port_make_error_from_context(struct redisAsyncContext *context, hierdis_port_spec_t *spec);
static int	hierdis_port_make_term_from_reply(redisReply *reply, hierdis_port_spec_t *spec);
static int	hierdis_port_make_binary_from_reply(redisReply *reply, hierdis_port_spec_t *spec);
static int	hierdis_port_make_list_from_reply(redisReply *reply, hierdis_port_spec_t *spec);
static int	hierdis_port_make_integer_from_reply(redisReply *reply, hierdis_port_spec_t *spec);

#endif
