%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet

-module(redis_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([
    strings/1,
    hashes/1,
    sets/1,
    pubsub/1
]).

all() ->
    [
        {group, tcp_sync},
        {group, unix_sync},
        {group, tcp_async},
        {group, unix_async}
    ].

groups() ->
    Tests = [
        strings,
        hashes,
        sets
    ],
    [
        {tcp_sync, [parallel], Tests},
        {unix_sync, [parallel], Tests},
        {tcp_async, [parallel], Tests ++ [pubsub]},
        {unix_async, [parallel], Tests ++ [pubsub]}
    ].

init_per_suite(Config) ->
	application:start(redis_test_server),
    application:start(hierdis),
    %% Ensure hierdis module is loaded (and the NIF)
    {error, _} = hierdis:connect("127.0.0.1", 0),
    Config.

end_per_suite(_Config) ->
    application:stop(redis_test_server),
    application:stop(hierdis),
    ok.

init_per_group(Name, Config) ->
	ct:log("starting ~s server...", [Name]),
	Options = redis_options_for_group(Name),
	{ok, _Pid} = redis_test_server:start_listener(Name, Options),
	ct:log("started"),
	[{redis_ref, Name} | Config].

end_per_group(Name, _Config) ->
	ct:log("stopping ~s server...", [Name]),
	redis_test_server:stop_listener(Name),
	ct:log("stopped"),
	ok.

%%====================================================================
%% Tests
%%====================================================================

strings(Config) ->
	Name = ?config(redis_ref, Config),
	{Mod, Client} = client_for_group(Name),
	{ok, undefined} = Mod:command(Client, ["GET", "key"]),
	{ok, <<"OK">>} = Mod:command(Client, ["SET", "key", "val"]),
	{ok, <<"val">>} = Mod:command(Client, ["GET", "key"]),
	{ok, 1} = Mod:command(Client, ["DEL", "key"]),
	ok.

hashes(Config) ->
	Name = ?config(redis_ref, Config),
	{Mod, Client} = client_for_group(Name),
	{ok, undefined} = Mod:command(Client, ["HGET", "hash", "field"]),
	{ok, []} = Mod:command(Client, ["HGETALL", "hash"]),
	{ok, 1} = Mod:command(Client, ["HSET", "hash", "field", "val"]),
	{ok, <<"val">>} = Mod:command(Client, ["HGET", "hash", "field"]),
	{ok, [<<"field">>, <<"val">>]} = Mod:command(Client, ["HGETALL", "hash"]),
	{ok, 1} = Mod:command(Client, ["DEL", "hash"]),
	ok.

sets(Config) ->
	Name = ?config(redis_ref, Config),
	{Mod, Client} = client_for_group(Name),
	{ok, []} = Mod:command(Client, ["SMEMBERS", "set"]),
	{ok, 1} = Mod:command(Client, ["SADD", "set", "val"]),
	{ok, [<<"val">>]} = Mod:command(Client, ["SMEMBERS", "set"]),
	{ok, 1} = Mod:command(Client, ["DEL", "set"]),
	ok.

pubsub(Config) ->
	Name = ?config(redis_ref, Config),
	{Mod, Publisher} = client_for_group(Name),
	{Mod, Subscriber} = client_for_group(Name),
	{ok, [<<"subscribe">>, <<"channel">>, 1]} = Mod:command(Subscriber, ["SUBSCRIBE", "channel"]),
	{ok, 1} = Mod:command(Publisher, ["PUBLISH", "channel", "test"]),
	receive
		{redis_message, Subscriber, [<<"message">>, <<"channel">>, <<"test">>]} ->
			ok
	after
		1000 ->
			ct:fail("Subscriber should have received \"test\" publish, but did not.")
	end,
	{ok, [<<"unsubscribe">>, <<"channel">>, 0]} = Mod:command(Subscriber, ["UNSUBSCRIBE", "channel"]),
	ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% @private
client_for_group(tcp_sync) ->
	{ok, Client} = hierdis:connect("127.0.0.1", redis_test_server:get_port(tcp_sync)),
	{hierdis, Client};
client_for_group(unix_sync) ->
	{ok, Client} = hierdis:connect_unix(redis_test_server:get_path(unix_sync)),
	{hierdis, Client};
client_for_group(tcp_async) ->
	{ok, Client} = hierdis_async:connect("127.0.0.1", redis_test_server:get_port(tcp_async)),
	{hierdis_async, Client};
client_for_group(unix_async) ->
	{ok, Client} = hierdis_async:connect_unix(redis_test_server:get_path(unix_async)),
	{hierdis_async, Client}.

%% @private
redis_options_for_group(Group) when Group =:= tcp_sync orelse Group =:= tcp_async ->
	[{tcp, true}];
redis_options_for_group(Group) when Group =:= unix_sync orelse Group =:= unix_async ->
	[{unix, true}].
