-module(basho_bench_driver_hierdis).

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
    		{ok, Context} = hierdis:connect_unix(SocketPath),
            Result = {ok, #state{
                redis_context=Context,
                pipe_length=PipelineLength
            }};
    	{tcp, {Host, Port}} ->
            {ok, Context} = hierdis:connect(Host, Port),
            Result = {ok, #state{
                redis_context=Context,
                pipe_length=PipelineLength
            }}
    end,
    Result.

run(get, KeyGen, _ValueGen, #state{redis_context=Context}=State) ->
    Start = KeyGen(),
    case hierdis:command(Context, [<<"GET">>, Start]) of
        {ok, _Value} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(put, KeyGen, ValueGen, #state{redis_context=Context}=State) ->
    case hierdis:command(Context, [<<"SET">>, KeyGen(), ValueGen()]) of
        {ok, <<"OK">>} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(delete, KeyGen, _ValueGen, #state{redis_context=Context}=State) ->
    Start = KeyGen(),
    case hierdis:command(Context, [<<"DEL">>, Start]) of
        {ok, _Response} ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(pipeline_get, KeyGen, _ValueGen, #state{redis_context=Context,pipe_length=PipelineLength}=State) ->
    Seq = lists:seq(1, PipelineLength),
    Pipe = [[<<"GET">>, KeyGen()] || _ <- Seq],

    case hierdis:pipeline(Context, Pipe) of
        [_First|_Rest] ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(pipeline_put, KeyGen, ValueGen, #state{redis_context=Context,pipe_length=PipelineLength}=State) ->
    Seq = lists:seq(1, PipelineLength),
    Pipe = [["SET", KeyGen(), ValueGen()] || _ <- Seq],

    case hierdis:pipeline(Context, Pipe) of
        [_First|_Rest] ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end;

run(pipeline_delete, KeyGen, _ValueGen, #state{redis_context=Context,pipe_length=PipelineLength}=State) ->
    Seq = lists:seq(1, PipelineLength),
    Pipe = [[<<"DEL">>, KeyGen()] || _ <- Seq],

    case hierdis:pipeline(Context, Pipe) of
        [_First|_Rest] ->
            {ok, State};
        {error, Reason} ->
            {error, Reason, State}
    end.


