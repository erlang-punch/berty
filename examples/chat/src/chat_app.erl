%%%===================================================================
%%% @copyright 2023 (c) Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%====================================================================
-module(chat_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = ranch:start_listener(chat_tcp, ranch_tcp, [{port, 7777}], chat_protocol, []),
    chat_sup:start_link().

stop(_State) ->
    ranch:stop_listener(chat_tcp),
    ok.

