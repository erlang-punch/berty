%%%===================================================================
%%% @copyright 2023 (c) Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%====================================================================
-module(chat_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = children(),
    {ok, {SupFlags, ChildSpecs}}.

children() -> 
    [chat_event_manager()
    ,chat_event_handler()
    ].

chat_event_manager() ->
    #{ id => chat_event_manager
     , start => {gen_event, start_link, [{local, chat_event_manager}]}
     , type => worker
     }.

chat_event_handler() ->
    #{ id => chat_event_handler
     , start => {chat_event_handler, start_link, []}
     , type => worker
     , restart => temporary
     }.
