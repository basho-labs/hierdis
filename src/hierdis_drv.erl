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

-module(hierdis_drv).
-author('Andrew Bennett <andrew@pagodabox.com>').

%% API
-export([load/0, unload/0]).

-define(DRIVER_NAME, ?MODULE_STRING).

%%%===================================================================
%%% API
%%%===================================================================

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

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
priv_dir() ->
	case code:priv_dir(?MODULE) of
		{error, bad_name} ->
			case code:which(hierdis) of
				Filename when is_list(Filename) ->
					filename:join([filename:dirname(Filename), "../priv"]);
				_ ->
					"../priv"
			end;
		Dir ->
			Dir
	end.
