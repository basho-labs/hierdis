%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet

% (The MIT License)

% Copyright (c) 2013 Andrew Bennett

% Permission is hereby granted, free of charge, to any person obtaining
% a copy of this software and associated documentation files (the
% 'Software'), to deal in the Software without restriction, including
% without limitation the rights to use, copy, modify, merge, publish,
% distribute, sublicense, and/or sell copies of the Software, and to
% permit persons to whom the Software is furnished to do so, subject to
% the following conditions:

% The above copyright notice and this permission notice shall be
% included in all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(hierdis_async).
-author('Andrew Bennett <andrew@pagodabox.com>').
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([connect/2, connect/3, connect_unix/1, connect_unix/2, close/1,
	command/2, pipeline/2, transaction/2, append_command/2, get_reply/1,
	controlling_process/2]).
-export([load/0, unload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include("hierdis.hrl").

-define(DRIVER_ATOM, 'hierdis_drv').
-define(DRIVER_NAME, "hierdis_drv").

-record(state, {
	port = undefined :: undefined | port()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link()
	-> {ok, pid()} | ignore | {error, term()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec connect(Ip::string(), Port::inet:port_number())
	-> {atom(), binary()} | error().
connect(Ip, Port) when Port > 0, Port =< 65535 ->
	connect(Ip, Port, infinity).

-spec connect(Ip::string(), Port::inet:port_number(), Timeout::infinity | timeout())
	-> {atom(), binary()} | error().
connect(Ip, Port, Timeout) when Port > 0, Port =< 65535 ->
	Args = case Timeout of
		infinity ->
			{Ip, Port};
		_ when is_integer(Timeout) andalso Timeout >= 0 ->
			{Ip, Port, Timeout}
	end,
	case load() of
		ok ->
			Socket = erlang:open_port({spawn_driver, ?DRIVER_NAME}, [binary]),
			try erlang:port_call(Socket, ?HIERDIS_CALL_CONNECT, Args) of
				ok ->
					receive
						{redis_opened, Socket} ->
							{ok, Socket};
						{redis_error, Socket, Error} ->
							close(Socket),
							receive
								{redis_closed, Socket} ->
									{error, Error}
							after
								0 ->
									{error, Error}
							end
					end;
				ConnectError ->
					close(Socket),
					ConnectError
			catch
				error:badarg ->
					close(Socket),
					{error, closed}
			end;
		LoadError ->
			LoadError
	end.

-spec connect_unix(SocketPath::string())
	-> {atom(), binary()} | error().
connect_unix(SocketPath) ->
	connect_unix(SocketPath, infinity).

-spec connect_unix(SocketPath::string(), Timeout::infinity | timeout())
	-> {atom(), binary()} | error().
connect_unix(SocketPath, Timeout) ->
	Args = case Timeout of
		infinity ->
			{SocketPath};
		_ when is_integer(Timeout) andalso Timeout >= 0 ->
			{SocketPath, Timeout}
	end,
	case load() of
		ok ->
			Socket = erlang:open_port({spawn_driver, ?DRIVER_NAME}, [binary]),
			try erlang:port_call(Socket, ?HIERDIS_CALL_CONNECT_UNIX, Args) of
				ok ->
					receive
						{redis_opened, Socket} ->
							{ok, Socket};
						{redis_error, Socket, Error} ->
							close(Socket),
							receive
								{redis_closed, Socket} ->
									{error, Error}
							after
								0 ->
									{error, Error}
							end
					end;
				ConnectError ->
					close(Socket),
					ConnectError
			catch
				error:badarg ->
					close(Socket),
					{error, closed}
			end;
		LoadError ->
			LoadError
	end.

-spec close(Socket::port())
	-> ok | {error, any()}.
close(Socket) ->
	case erlang:port_info(Socket) of
		undefined ->
			ok;
		_ ->
			case controlling_process(Socket, self()) of
				ok ->
					receive
						{redis_closed, Socket} -> % already closed
							catch erlang:port_close(Socket),
							self() ! {redis_closed, Socket},
							ok
					after
						0 ->
							catch erlang:port_close(Socket),
							ok
					end;
				ControlError ->
					ControlError
			end
	end.

-spec command(Socket::port(), CommandArgs::iolist())
	-> {atom(), term()} | error().
command(Socket, CommandArgs) ->
	case controlling_process(Socket, self()) of
		ok ->
			try append_command(Socket, CommandArgs) of
				{ok, _} ->
					get_reply(Socket);
				CommandError ->
					CommandError
			catch
				error:badarg ->
					close(Socket),
					{error, closed}
			end;
		ControlError ->
			ControlError
	end.

-spec pipeline(Socket::port(), CommandList::iolist())
	-> {atom(), list(term())} | error().
pipeline(Socket, CommandList) ->
	case controlling_process(Socket, self()) of
		ok ->
			PipelineLength = pipe_builder(pipeline, Socket, CommandList, 0),
			pipe_cleaner(pipeline, Socket, [], PipelineLength);
		ControlError ->
			ControlError
	end.

-spec transaction(Socket::port(), CommandList::iolist())
	-> {atom(), list(term())} | error().
transaction(Socket, CommandList) ->
	TransactionLength = pipe_builder(transaction, Socket, CommandList, 0),
	pipe_cleaner(transaction, Socket, [], TransactionLength).

-spec append_command(Socket::port(), CommandArgs::iolist())
	-> {atom(), integer()} | error().
append_command(Socket, CommandArgs) ->
	case erlang:port_call(Socket, ?HIERDIS_CALL_COMMAND, CommandArgs) of
		{ok, N} when is_integer(N) ->
			{ok, N};
		CommandError ->
			{error, CommandError}
	end.

-spec get_reply(Socket::port())
	-> {atom(), term()} | error().
get_reply(Socket) ->
	receive
		{redis_message, Socket, {error, RedisMessageError}} ->
			{error, RedisMessageError};
		{redis_message, Socket, Message} ->
			{ok, Message};
		{redis_reply, Socket, {error, RedisReplyError}} ->
			{error, RedisReplyError};
		{redis_reply, Socket, Reply} ->
			{ok, Reply};
		{redis_error, Socket, RedisError} ->
			{error, RedisError};
		{redis_closed, Socket} ->
			{error, closed}
	end.

-spec controlling_process(Socket::port(), Pid::pid())
	-> ok | {error, Reason::any()}.
controlling_process(Socket, NewOwner) when is_port(Socket), is_pid(NewOwner) ->
	case erlang:port_info(Socket, connected) of
		{connected, NewOwner} ->
			ok;
		{connected, Pid} when Pid =/= self() ->
			{error, not_owner};
		undefined ->
			{error, einval};
		_ ->
			case sync_input(Socket, NewOwner, false) of
				true ->
					ok;
				false ->
					try erlang:port_connect(Socket, NewOwner) of
						true ->
							erlang:unlink(Socket), %% unlink from port
							ok
					catch
						error:Reason ->
							{error, Reason}
					end
			end
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc Load port driver
%% @spec load() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
load() ->
	{ok, Drivers} = erl_ddll:loaded_drivers(),
	case lists:member(?DRIVER_NAME, Drivers) of
		true ->
			ok;
		false ->
			case erl_ddll:load(priv_dir(), ?DRIVER_NAME) of
				ok ->
					ok;
				{error, already_loaded} ->
					ok;
				{error, Error} ->
					error_logger:error_msg(
						?MODULE_STRING ": Error loading ~p: ~p~n",
						[?DRIVER_NAME, erl_ddll:format_error(Error)]
					),
					{error, Error}
			end
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc Unload port driver
%% @spec unload() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
unload() ->
	case erl_ddll:unload_driver(?DRIVER_NAME) of
		ok ->
			ok;
		{error, Error} ->
			error_logger:error_msg(
				?MODULE_STRING ": Error unloading ~p: ~p~n",
				[?DRIVER_NAME, erl_ddll:format_error(Error)]
			),
			{error, Error}
	end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([])
	-> ignore | {ok, #state{}} | {stop, any()}.
init([]) ->
	erlang:process_flag(trap_exit, true),
	case load() of
		ok ->
			Port = erlang:open_port({spawn_driver, ?DRIVER_NAME}, [binary]),
			erlang:register(?DRIVER_ATOM, Port),
			State = #state{port=Port},
			{ok, State};
		{error, LoadError} ->
			LoadErrorStr = erl_ddll:format_error(LoadError),
			ErrorStr = lists:flatten(io_lib:format(
				"could not load driver ~s: ~p",
				[?DRIVER_NAME, LoadErrorStr])),
			{stop, ErrorStr}
	end.

-spec handle_call(any(), {pid(), any()}, #state{})
	-> {reply, any(), #state{}}.
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

-spec handle_cast(any(), #state{})
	-> {noreply, #state{}} | {stop, any(), #state{}}.
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

-spec handle_info(any(), #state{})
	-> {noreply, #state{}}.
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(any(), #state{})
	-> ok.
terminate(_Reason, #state{port=Port}) ->
	erlang:unregister(?DRIVER_ATOM),
	erlang:port_close(Port),
	ok.

-spec code_change(any(), #state{}, any())
	-> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
pipe_builder(pipeline, _Socket, [], Counter) ->
	Counter;
pipe_builder(transaction, Socket, [Command|List], 0) ->
	append_command(Socket, [?TRANSACTION_BEGIN]),
	append_command(Socket, Command),
	pipe_builder(transaction, Socket, List, 2);
pipe_builder(transaction, Socket, [], Counter) ->
	append_command(Socket, [?TRANSACTION_END]),
	Counter+1;
pipe_builder(Scheme, Socket, [Command|List], Counter) ->
	append_command(Socket, Command),
	pipe_builder(Scheme, Socket, List, Counter+1).

%% @private
pipe_cleaner(pipeline, _Socket, Acc, 0) ->
	lists:reverse(Acc);
pipe_cleaner(transaction, Socket, _Acc, 1) ->
	get_reply(Socket);
pipe_cleaner(Scheme, Socket, Acc, Counter) ->
	pipe_cleaner(Scheme, Socket, [get_reply(Socket)|Acc], Counter-1).

%% @private
priv_dir() ->
	case code:priv_dir(hierdis) of
		{error, bad_name} ->
			case code:which(?MODULE) of
				Filename when is_list(Filename) ->
					filename:join([filename:dirname(Filename), "../priv"]);
				_ ->
					"../priv"
			end;
		Dir ->
			Dir
	end.

%% @private
sync_input(Socket, Owner, Flag) ->
	receive
		{redis_closed, Socket} ->
			Owner ! {redis_closed, Socket},
			sync_input(Socket, Owner, true);
		{redis_error, Socket, Reason} ->
			Owner ! {redis_error, Socket, Reason},
			sync_input(Socket, Owner, Flag);
		{redis_message, Socket, Message} ->
			Owner ! {redis_message, Socket, Message},
			sync_input(Socket, Owner, Flag);
		{redis_opened, Socket} ->
			Owner ! {redis_opened, Socket},
			sync_input(Socket, Owner, Flag);
		{redis_reply, Socket, Reply} ->
			Owner ! {redis_reply, Socket, Reply},
			sync_input(Socket, Owner, Flag)
	after
		0 ->
			Flag
	end.
