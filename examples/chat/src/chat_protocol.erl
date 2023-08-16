%%%===================================================================
%%% @copyright 2023 (c) Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%====================================================================
-module(chat_protocol).
-behaviour(ranch_protocol).
-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
            route(Socket, Transport, Data);
        {error, timeout} ->
            gen_event:notify(chat_event_manager, {connection, timeout, Socket}),
            ok = Transport:close(Socket);
        {error, closed} ->
            gen_event:notify(chat_event_manager, {connection, closed, Socket}),
            ok = Transport:close(Socket);
        Message ->
            io:format("~p~n", [{Socket, Transport, Message}]),
            ok = Transport:close(Socket)
    end.

route(Socket, Transport, Data) ->
    case parse(Socket, Transport, Data) of
        {ok, Parsed} ->
            gen_event:notify(chat_event_manager, Parsed),
            loop(Socket, Transport);
        Elsewise ->
            gen_event:notify(chat_event_manager, Elsewise),
            ok = Transport:close(Socket)
    end.

parse(Socket, Transport, Data) ->
    try 
        Term = erlang:binary_to_term(Data),
        parse_map(Socket, Transport, Term)
    catch
        _:R -> {error, R}
    end.
    
parse_map(Socket, _, #{ <<"message">> := Message, <<"to">> := User } = Map) 
  when is_binary(Message) andalso is_binary(User) ->
    {ok, {message, Socket, User, Message}};
parse_map(Socket, _, #{ <<"user">> := User } = Map) 
  when is_binary(User) ->
    {ok, {user, Socket, User}};
parse_map(_, _, _) ->
    {error, bad_map}.
