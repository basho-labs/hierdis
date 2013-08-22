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

#include <erl_driver.h>
#include <erl_interface.h>
#include <string.h>
#include "hierdis_port.h"

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

hierdis_call_t	*hierdis_call_new(hierdis_port_t *port, unsigned int command, char *buf, ErlDrvSizeT len, char *rbuf, ErlDrvSizeT rlen);
void		hierdis_call_free(hierdis_call_t *call);
void		hierdis_call_execute(hierdis_call_t *call);

void	hierdis_call_connect(hierdis_call_t *call);
void	hierdis_call_connect_unix(hierdis_call_t *call);

/*
 * Error functions
 */
void		hierdis_call_badarg(hierdis_call_t *call);
void		hierdis_call_make_error(hierdis_call_t *call, char *atom, int atom_needs_free, char *reason, int reason_needs_free);

/*
 * Call Error functions
 */
hierdis_call_error_t	*hierdis_call_error_new(hierdis_call_t *call, char *atom, int atom_needs_free, char *reason, int reason_needs_free);
void			hierdis_call_error_free(hierdis_call_error_t *error);
void			hierdis_call_error_write(hierdis_call_error_t *error);

#endif
