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

#ifndef __HIREDIS_ERL_DRIVER_H__
#define __HIREDIS_ERL_DRIVER_H__

#include "hierdis_drv_common.h"

typedef struct redisErlDriverEvents {
	redisAsyncContext	*context;
	ErlDrvData		drv_data;
	ErlDrvPort		drv_port;
	ErlDrvEvent		drv_event;
	int			events;
	int			kill;
} redisErlDriverEvents;

extern void	redisErlDriverAddRead(void *privdata);
extern void	redisErlDriverDelRead(void *privdata);
extern void	redisErlDriverAddWrite(void *privdata);
extern void	redisErlDriverDelWrite(void *privdata);
extern void	redisErlDriverCleanup(void *privdata);
extern int	redisErlDriverAttach(redisAsyncContext *ac, ErlDrvPort port, ErlDrvData data);

#endif
