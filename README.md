## Overview

High-performance Erlang client for the Redis key-value store.

hierdis presents a simple API similar to the synchronous API exposed by the well-known [hiredis](https://github.com/redis/hiredis) C client.  This is done by exposing hiredis C client functionality to Erlang through the NIF interface.


## Installation

**Pre-requisites:** You must already have Erlang R15B01 or later installed on your machine.

	$ git clone git@github.com:nathanaschbacher/hierdis.git .
	$ cd hierdis
	$ ./rebar compile
	
This should automatically build the `hiredis` dependencies and move the headers and libraries to the `priv/` directory.


## Usage 

Get a Redis connection.

    $ erl -pa ebin/
    
    Eshell V5.9.1  (abort with ^G)
	1> {ok,C} = hierdis:connect_unix("/tmp/redis.sock").
	{ok,<<>>}

Issue commands to Redis as an `iolist`.

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

Pipeline commands to Redis and get the replies.

    8> hierdis:append_command(C, ["MULTI"]).
    {ok,15}
    9> hierdis:append_command(C, ["SET", "foo", "pipelined"]).
    {ok,52}
    10> hierdis:append_command(C, ["SET", "bar", "linedpipe"]).
    {ok,89}
    11> hierdis:append_command(C, ["SET", "baz", "ploplooned"]).
    {ok,128}
    12> hierdis:append_command(C, ["MGET", "foo", "bar", "baz"]).
    {ok,169}
    13> hierdis:append_command(C, ["EXEC"]).
    {ok,183}
    14> hierdis:get_reply(C).
    {ok,<<"OK">>}
    15> hierdis:get_reply(C).
    {ok,<<"QUEUED">>}
    16> hierdis:get_reply(C).
    {ok,<<"QUEUED">>}
    17> hierdis:get_reply(C).
    {ok,<<"QUEUED">>}
    18> hierdis:get_reply(C).
    {ok,<<"QUEUED">>}
    19> hierdis:get_reply(C).
    {ok,[<<"OK">>,<<"OK">>,<<"OK">>,
         [<<"pipelined">>,<<"linedpipe">>,<<"ploplooned">>]]}

Simple.

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