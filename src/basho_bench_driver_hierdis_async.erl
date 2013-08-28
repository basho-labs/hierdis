%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet

-module(basho_bench_driver_hierdis_async).

-export([new/1,
		 run/4]).

-record(state, {redis_context :: term(),
				pipe_length :: integer()}).

new(_Id) ->
	application:start(hierdis),

	ConnectionParams = basho_bench_config:get(connect, {socket, "/tmp/redis.sock"}),
	PipelineLength = basho_bench_config:get(pipeline_length, 10),
	
	case ConnectionParams of
		{socket, SocketPath} ->
			{ok, Context} = hierdis_async:connect_unix(SocketPath),
			Result = {ok, #state{
				redis_context=Context,
				pipe_length=PipelineLength
			}};
		{tcp, {Host, Port}} ->
			{ok, Context} = hierdis_async:connect(Host, Port),
			Result = {ok, #state{
				redis_context=Context,
				pipe_length=PipelineLength
			}}
	end,
	Result.

run(get, KeyGen, _ValueGen, #state{redis_context=Context}=State) ->
	Start = KeyGen(),
	case hierdis_async:command(Context, [<<"GET">>, Start]) of
		{ok, _Value} ->
			{ok, State};
		{error, Reason} ->
			{error, Reason, State}
	end;

run(put, KeyGen, ValueGen, #state{redis_context=Context}=State) ->
	case hierdis_async:command(Context, [<<"SET">>, KeyGen(), ValueGen()]) of
		{ok, <<"OK">>} ->
			{ok, State};
		{error, Reason} ->
			{error, Reason, State}
	end;

run(delete, KeyGen, _ValueGen, #state{redis_context=Context}=State) ->
	Start = KeyGen(),
	case hierdis_async:command(Context, [<<"DEL">>, Start]) of
		{ok, _Response} ->
			{ok, State};
		{error, Reason} ->
			{error, Reason, State}
	end.
