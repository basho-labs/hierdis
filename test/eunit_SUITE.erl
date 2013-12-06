%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet

-module(eunit_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).

%% Tests.
-export([eunit/1]).

%% ct.

all() ->
	[eunit].

eunit(_Config) ->
	ok = eunit:test({application, hierdis}).
