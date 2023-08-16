%%%===================================================================
%%% @copyright 2023 (c) Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%====================================================================
-module(chat_manager).
-behavior(gen_server).
-compile(export_all).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #{}}.

handle_cast({user, add, Socket, Name} = Msg, State) ->
    io:format("~p~n", [{?MODULE, Msg}]),
    NewState = maps:put({Socket, Name}, active, State),
    {noreply, NewState};
handle_cast({user, delete, Socket} = Msg, State) 
  when is_port(Socket) ->
    io:format("~p~n", [{?MODULE, Msg}]),
    Fun = fun({Socket,_}, _) -> false; (_,_) -> true end,
    NewState = maps:filter(Fun, State),
    {noreply, NewState};
handle_cast({user, delete, User} = Msg, State)
  when is_binary(User) ->
    io:format("~p~n", [{?MODULE, Msg}]),
    Fun = fun({_,User}, _) -> false; (_,_) -> true end,
    NewState = maps:filter(Fun, State),
    {noreply, NewState}.

handle_call({user, get, Socket} = Msg, _, State) 
  when is_port(Socket) ->
    io:format("~p~n", [{?MODULE, Msg}]),
    Fun = fun({Socket,User}, _) -> true; (_,_) -> false end,
    Reply = case maps:filter(Fun, State) of
                Map when map_size(Map) =:= 0 -> 
                    {error, notfound};
                Elsewise -> 
                    {ok, maps:keys(Elsewise)}
            end,
    {reply, Reply, State};
handle_call({user, get, User} = Msg, _, State) 
  when is_binary(User) ->
    io:format("~p~n", [{?MODULE, Msg}]),
    Fun = fun({Socket,User}, _) -> true; (_,_) -> false end,
    Reply = case maps:filter(Fun, State) of
                Map when map_size(Map) =:= 0 -> 
                    {error, notfound};
                Elsewise -> 
                    {ok, maps:keys(Elsewise)}
            end,
    {reply, Reply, State}.

user_add(Socket, Name) ->
    gen_server:cast(?MODULE, {user, add, Socket, Name}).

user_delete(Identifier) ->
    gen_server:cast(?MODULE, {user, delete, Identifier}).

user_get(Identifier) ->
    gen_server:call(?MODULE, {user, get, Identifier}).

