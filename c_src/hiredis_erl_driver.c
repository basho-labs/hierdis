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

#include "hiredis_erl_driver.h"

void
redisErlDriverAddRead(void *privdata)
{
	redisErlDriverEvents *p;

	TRACE_F(("redisErlDriverAddRead:%s:%d\n", __FILE__, __LINE__));

	p = (redisErlDriverEvents *)privdata;
	p->events |= ERL_DRV_READ;
	p->events |= ERL_DRV_USE;
	driver_select(p->drv_port, p->drv_event, p->events, 1);
}

void
redisErlDriverDelRead(void *privdata)
{
	redisErlDriverEvents *p;

	TRACE_F(("redisErlDriverDelRead:%s:%d\n", __FILE__, __LINE__));

	p = (redisErlDriverEvents *)privdata;
	p->events &= ~ERL_DRV_READ;
	driver_select(p->drv_port, p->drv_event, ERL_DRV_READ, 0);
}

void
redisErlDriverAddWrite(void *privdata)
{
	redisErlDriverEvents *p;

	TRACE_F(("redisErlDriverAddWrite:%s:%d\n", __FILE__, __LINE__));

	p = (redisErlDriverEvents *)privdata;
	p->events |= ERL_DRV_WRITE;
	p->events |= ERL_DRV_USE;
	driver_select(p->drv_port, p->drv_event, p->events, 1);
}

void
redisErlDriverDelWrite(void *privdata)
{
	redisErlDriverEvents *p;

	TRACE_F(("redisErlDriverDelWrite:%s:%d\n", __FILE__, __LINE__));

	p = (redisErlDriverEvents *)privdata;
	p->events &= ~ERL_DRV_WRITE;
	driver_select(p->drv_port, p->drv_event, ERL_DRV_WRITE, 0);
}

void
redisErlDriverCleanup(void *privdata)
{
	redisErlDriverEvents *p;
	ErlDrvPort drv_port;
	ErlDrvEvent drv_event;
	int events;
	int kill;

	TRACE_F(("redisErlDriverCleanup:%s:%d\n", __FILE__, __LINE__));

	if (privdata == NULL) {
		return;
	}

	p = (redisErlDriverEvents *)privdata;
	p->events |= ERL_DRV_USE;
	drv_port = p->drv_port;
	drv_event = p->drv_event;
	events = p->events;
	kill = p->kill;
	p->context->ev.data = NULL;
	(void) driver_free(p);
	p = NULL;
	driver_select(drv_port, drv_event, events, 0);
	if (kill == 1) {
		driver_failure_eof(drv_port);
	}
}

int
redisErlDriverAttach(redisAsyncContext *ac, ErlDrvPort port, ErlDrvData data)
{
	redisContext *c;
	redisErlDriverEvents *p;

	TRACE_F(("redisErlDriverAttach:%s:%d\n", __FILE__, __LINE__));

	c = &(ac->c);

	if (ac->ev.data != NULL) {
		return REDIS_ERR;
	}

	ac->ev.addRead  = redisErlDriverAddRead;
	ac->ev.delRead  = redisErlDriverDelRead;
	ac->ev.addWrite = redisErlDriverAddWrite;
	ac->ev.delWrite = redisErlDriverDelWrite;
	ac->ev.cleanup  = redisErlDriverCleanup;

	p = (redisErlDriverEvents *)(driver_alloc(sizeof (redisErlDriverEvents)));

	if (!p) {
		return REDIS_ERR;
	}

	memset(p, 0, sizeof(*p));

	ac->ev.data  = p;
	p->context   = ac;
	p->drv_data  = data;
	p->drv_port  = port;
	p->drv_event = (ErlDrvEvent)(c->fd);

	return REDIS_OK;
}
