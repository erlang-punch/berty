%%%===================================================================
%%% @copyright 2023 (c) Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%====================================================================
-module(chat_event_handler).
-behaviour(gen_event).
-export([start_link/0, stop/0]).
-export([init/1, handle_event/2, terminate/2]).

start_link() ->
    Opts = [chat_event_manager, ?MODULE, []],
    Pid = spawn_link(gen_event, add_handler, Opts),
    {ok, Pid}.

stop() ->
    gen_event:delete_handler(chat_event_manager, ?MODULE).

init(_Args) ->
    {ok, []}.

handle_event({user, Socket, User}, State) ->
    chat_manager:user_add(Socket, User),
    {ok, State};
handle_event({message, From, To, Message} = Msg, State) ->
    io:format("~p~n", [{?MODULE, Msg}]),
    case chat_manager:user_get(To) of
        {ok, [{Socket,_}]} ->
            ranch_tcp:send(Socket, Message),
            {ok, State};
        _ ->
            {ok, state}
    end;
handle_event({connection, timeout, Socket}, State) ->
    chat_manager:user_delete(Socket),
    {ok, State};
handle_event({connection, closed, Socket}, State) ->
    chat_manager:user_delete(Socket),
    {ok, State};
handle_event(Message, State) ->
    io:format("~p~n", [{?MODULE, Message, State}]),
    {ok, State}.

terminate(_Args, _State) ->
    ok.
