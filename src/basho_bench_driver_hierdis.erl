-module(basho_bench_driver_hierdis).

-export([new/1,
         run/4]).

new(_Id) ->
    application:start(hierdis),

    ConnectionParams = basho_bench_config:get(connect, {socket, "/tmp/redis.sock"}),
    
    case ConnectionParams of
    	{socket, SocketPath} ->
    		hierdis:connect_unix(SocketPath);
    	{tcp, {Host, Port}} ->
    		hierdis:connect(Host, Port)
    end.

run(get, KeyGen, _ValueGen, Context) ->
    Start = KeyGen(),
    case hierdis:command(Context, [<<"GET">>, Start]) of
        {ok, _Value} ->
            {ok, Context};
        {error, Reason} ->
            {error, Reason, Context}
    end;

run(put, KeyGen, ValueGen, Context) ->
    case hierdis:command(Context, [<<"SET">>, KeyGen(), ValueGen()]) of
        {ok, <<"OK">>} ->
            {ok, Context};
        {error, Reason} ->
            {error, Reason, Context}
    end;

run(pipeline_get, KeyGen, _ValueGen, Context) ->
    Seq = lists:seq(1, 5),
    Pipe = [[<<"GET">>, KeyGen()] || _ <- Seq],

    case hierdis:pipeline(Context, Pipe) of
        {ok, _Results} ->
            {ok, Context};
        {error, Reason} ->
            {error, Reason, Context}
    end;

run(pipeline_put, KeyGen, ValueGen, Context) ->
    Seq = lists:seq(1, 5),
    Pipe = [["SET", KeyGen(), ValueGen()] || _ <- Seq],
    Results = [{ok, <<"OK">>} || _ <- Seq],

    case hierdis:pipeline(Context, Pipe) of
        Results ->
            {ok, Context};
        {error, Reason} ->
            {error, Reason, Context}
    end.




