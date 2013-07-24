## Overview

High-performance Erlang client for the Redis key-value store.

hierdis presents a simple API similar to the synchronous API exposed by the well-known [hiredis](https://github.com/redis/hiredis) C client.  This is done by exposing hiredis C client functionality to Erlang through the NIF interface.  Using this approach also allows hierdis to communicate with Redis via unix domain sockets, which can provide a 50% increase in throughput over TCP.

## Installation

**Pre-requisites:** You must already have Erlang R15B01 or later installed on your machine.

	$ git clone git@github.com:nathanaschbacher/hierdis.git .
	$ cd hierdis
	$ ./rebar compile
	
This should automatically build the `hiredis` dependencies and move the headers and libraries to the `priv/` directory.


## Usage 

#####Get a Redis connection.

    $ erl -pa ebin/
    
    Eshell V5.9.1  (abort with ^G)
	1> {ok,C} = hierdis:connect_unix("/tmp/redis.sock").
	{ok,<<>>}

#####Issue single commands to Redis as an `iolist`.

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

#####Pipeline commands to Redis as a `list` of `iolist`s.

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

#####Execute a transaction pipeline against Redis as a `list` of `iolist`s.

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
    
#####Manually append commands and get replies. 
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
