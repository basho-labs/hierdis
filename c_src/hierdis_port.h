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

#include <erl_driver.h>
#include <erl_interface.h>
#include <string.h>
#include "hiredis_erl_driver.h"

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

hierdis_port_t	*hierdis_port_new(ErlDrvPort erl_port);
void		hierdis_port_free(hierdis_port_t *port);
void		hierdis_port_read(hierdis_port_t *port);
void		hierdis_port_write(hierdis_port_t *port);

#endif
