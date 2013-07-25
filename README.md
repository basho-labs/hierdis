## Overview

High-performance Erlang client for the Redis key-value store.

hierdis presents a simple API similar to the synchronous API exposed by the well-known [hiredis](https://github.com/redis/hiredis) C client.  This is done by exposing hiredis C client functionality to Erlang through the NIF interface.  

Using this approach also allows hierdis to communicate with Redis via unix domain sockets, which by itself can provide a 50% increase in throughput over TCP.

## Installation

**Pre-requisites:** You must already have Erlang R15B01 or later installed on your machine.

	$ git clone git@github.com:nathanaschbacher/hierdis.git .
	$ cd hierdis
	$ ./rebar compile
	
This should automatically build the `hiredis` dependencies and move the headers and libraries to the `priv/` directory.

## Performance Comparison

On a MacBook Air equipped with a dual-core 2.0Ghz Core i7 CPU [hierdis](https://github.com/nathanaschbacher/hierdis) achieves __over 3x the throughput__ and sees __5x lower latency__ compared to [eredis](https://github.com/wooga/eredis) using 1 byte values.  When increasing the size of the stored values to 10 kb the performance of hierdis remained basically constant, and eredis performance cut in half.

### hierdis _(~40k ops/sec, ~0.06ms Avg. GET and PUT)_

![image](http://8f924b3a90f48795da10-9641d055ebc6aa017a8465b739bd1db3.r19.cf1.rackcdn.com/hierdis_5min_4workers/summary.png)

### eredis _(~12.5k ops/sec, ~0.30ms Avg. GET and PUT)_

![image](http://8f924b3a90f48795da10-9641d055ebc6aa017a8465b739bd1db3.r19.cf1.rackcdn.com/eredis_5min_4workers/summary.png)

Tests were performed using [basho_bench](https://github.com/basho/basho_bench) with identical configurations for both hierdis and eredis.  Except for utilizing their respective basho_bench drivers.  The GET/PUT balance was even, and the concurrency was set to 4 to match the number of logical cores on the CPU.

`redis-server` was flushed and restarted before each test run, and disk-persistence was disabled for each run as well.  

The respective configurations and complete results can be found here [hierdis_vs_eredis.zip](http://8f924b3a90f48795da10-9641d055ebc6aa017a8465b739bd1db3.r19.cf1.rackcdn.com/hierdis_vs_eredis.zip)

## Usage 

#####Get a Redis connection.

```erl
$ erl -pa ebin/

Eshell V5.9.1  (abort with ^G)
1> {ok,C} = hierdis:connect_unix("/tmp/redis.sock").
{ok,<<>>}
```

#####Issue single commands to Redis as an `iolist`.

```erl
2> hierdis:command(C, ["SET", "foo", "bar"]).
{ok,<<"OK">>}
3> hierdis:command(C, ["GET", "foo"]).
{ok,<<"bar">>}
4> hierdis:command(C, ["MSET" | ["key1", "1", "key2", "2", "key3", "3"]]).               
{ok,<<"OK">>}
5> hierdis:command(C, ["GET", "key1"]).                                                  
{ok,<<"1">>}
6> hierdis:command(C, ["GET", "key3"]).
{ok,<<"3">>}
7> hierdis:command(C, ["MGET" | ["key1", "key2", "key3"]]).  
{ok,[<<"1">>,<<"2">>,<<"3">>]}
```

#####Pipeline commands to Redis as a `list` of `iolist`s.

```erl
8> hierdis:pipeline(C, [
8>     ["SET", "foo", "bar"],
8>     ["GET", "foo"],
8>     ["MSET" | ["key1", "1", "key2", "2", "key3", "3"]],
8>     ["GET", "key1"],
8>     ["GET", "key3"],
8>     ["MGET" | ["key1", "key2", "key3"]]  
8> ]).
[{ok,<<"OK">>},
 {ok,<<"bar">>},
 {ok,<<"OK">>},
 {ok,<<"1">>},
 {ok,<<"3">>},
 {ok,[<<"1">>,<<"2">>,<<"3">>]}]
```

#####Execute a transaction pipeline against Redis as a `list` of `iolist`s.

```erl
9> hierdis:transaction(C, [ 
9>     ["SET", "foo", "bar"],
9>     ["GET", "foo"],
9>     ["MSET" | ["key1", "1", "key2", "2", "key3", "3"]],
9>     ["MGET" | ["key1", "key2", "key3"]]  
9> ]).
{ok,[<<"OK">>,<<"bar">>,<<"OK">>,[<<"1">>,<<"2">>,<<"3">>]]}
10> hierdis:transaction(C, [
10>     ["SET", "foo", "bar"],
10>     ["GET", "foo"],
10>     ["CRASHER!" | ["ka", "blooey"]],
10>     ["MGET" | ["key1", "key2", "key3"]]  
10> ]).
{error,{redis_reply_error,"EXECABORT Transaction discarded because of previous errors."}}
```

#####Manually append commands and get replies. 

```erl
11> hierdis:append_command(C, ["MULTI"]).
{ok,15}
12> hierdis:append_command(C, ["SET", "foo", "pipelined"]).
{ok,52}
13> hierdis:append_command(C, ["SET", "bar", "linedpipe"]).
{ok,89}
14> hierdis:append_command(C, ["EXEC"]).
{ok,183}
15> hierdis:get_reply(C).
{ok,<<"OK">>}
16> hierdis:get_reply(C).
{ok,<<"QUEUED">>}
17> hierdis:get_reply(C).
{ok,<<"QUEUED">>}
18> hierdis:get_reply(C).
{ok,[<<"OK">>,<<"OK">>,<<"OK">>,
     [<<"pipelined">>,<<"linedpipe">>,<<"ploplooned">>]]}
```

#Simple. Right?

## License

(The MIT License)

Copyright (c) 2013 Nathan Aschbacher

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
'Software'), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
