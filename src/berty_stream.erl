%%%===================================================================
%%% @copyright Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%%
%%% This module use stream bitstring and integers until the data is a
%%% valid ETF.
%%%
%%% ```
%%% {ok, P} = berty_stream:start(). 
%%% Binary = term_to_binary({1,2,3,4,test, {1, 2,3}, <<"mydata">>}).
%%% [ P ! X || <<X:8/bitstring>> <= Binary].
%%%
%%% % flush().
%%% Shell got {<0.3349.0>,{ok,{1,2,3,4,<<"test">>,{1,2,3},<<"mydata">>}}}
%%% '''
%%%
%%% A timeout is set by default to 100000 milliseconds (100 seconds),
%%% after this delay, the process stop and notify its parent.
%%%
%%% ```
%%% % check the timeout
%%% {ok, P} = berty_stream:start([{timeout, 100_000},{receiver, self()}]).
%%%
%%% % flush().
%%% % Shell got {<0.3345.0>,{timeout,<<>>}}
%%% '''
%%%
%%% At this time only `passive' mode is enabled.
%%%
%%%
%%% @TODO create an active version, to help developers to deal with
%%%       blocking system. Example: a probe using serialize
%%%       communication
%%% @TODO add support for ETS
%%% @end
%%%===================================================================
-module(berty_stream).
-behavior(gen_statem).
-export([start/0, start/1, start/2]).
-export([start_link/0, start_link/1, start_link/2]).
-export([start_monitor/0, start_monitor/1, start_monitor/2]).
-export([send/2, get/2]).
-export([stop/1]).
-export([callback_mode/0, init/1]).
-export([passive/3]).
-export([terminate/3]).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-record(?MODULE, { store   = undefined
                 , options = #{}
                 , decoder = #{}
                 , payload = <<>>
                 , term    = undefined 
                 , last    = undefined
                 , result  = undefined
                 , limit   = 0
                 , timer   = undefined
                 }).

%%--------------------------------------------------------------------
%% @doc
%% @see start/1
%% @end
%%--------------------------------------------------------------------
start() ->
    start([{receiver, self()}]).

%%--------------------------------------------------------------------
%% @doc
%% @see start/2
%% @end
%%--------------------------------------------------------------------
start(Args) ->
    start(Args, []).

%%--------------------------------------------------------------------
%% @doc start a new berty_stream process.
%% @end
%%--------------------------------------------------------------------
start(Args, Opts) ->
    Ref = make_ref(),
    gen_statem:start(?MODULE, [{reference,Ref}|Args], Opts).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([{receiver, self()}]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    start_link(Args, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_link(Args, Opts) ->
    Ref = make_ref(),
    gen_statem:start_link(?MODULE, [{reference,Ref}|Args], Opts).


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_monitor() ->
    start_monitor([{receiver, self()}]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_monitor(Args) ->
    start_monitor(Args, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
start_monitor(Args, Opts) ->
    Ref = make_ref(),
    gen_statem:start_monitor(?MODULE, [{reference,Ref}|Args], Opts).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
send(Pid, Message) ->
    erlang:send(Pid, Message).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
get(Pid, state) ->
    gen_statem:call(Pid, {get, state}, 10000).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
stop(Pid) ->
    gen_statem:stop(Pid).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    Ets = ets:new(?MODULE, [private, ordered_set]),
    Receiver = proplists:get_value(receiver, Args, undefined),
    Timeout = proplists:get_value(timeout, Args, 100_000),
    Notify = proplists:get_value(notify, Args, false),
    Decoder = proplists:get_value(decoder, Args, berty_etf:default_options()),
    Options = #{ timeout => Timeout
               , receiver => Receiver
               , notify => Notify
               },
    {ok, Timer} = timer:send_after(Timeout, self(), timeout),
    {ok, passive, #?MODULE{ store   = Ets
                          , options = Options 
                          , decoder = Decoder
                          , timer   = Timer
                          }}.
    
%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
terminate({shutdown, timeout}, passive, #?MODULE{ options = #{ receiver := Receiver}, payload = Payload }) ->
    Receiver ! {self(), {timeout, Payload}},
    ok;
terminate(normal, passive, #?MODULE{ options = #{ receiver := Receiver }
				   , term = Term }) ->
    ?LOG_DEBUG("~p",[{self(), terminate, normal, passive, Term}]),
    Receiver ! {self(), {ok, Term}},
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
passive(info, timeout, State) ->
    ?LOG_INFO("~p", [{timeout, self()}]),
    {stop, {shutdown, timeout}, State};
passive(info, <<X:8/unsigned-integer, _/binary>>, #?MODULE{ payload = <<>> } = State) 
  when X =/= 131 ->
    ?LOG_ERROR("~p", [{error, not_etf}]),
    {stop, {error, not_etf}, State};
passive(info, Message, State) 
  when is_bitstring(Message) ->
    decoder(Message, State);
passive(info, Message, State) 
  when is_integer(Message) ->
    decoder(<<Message:8/unsigned-integer>>, State);
% reset limit to 0 and re-initialize the payload
passive(cast, reset, State) ->
    {keep_state, State#?MODULE{ payload = <<>>, limit = 0 }};
passive({call, From}, {get, state}, State) ->    
    {keep_state, State, [{reply, From, State}]};
passive(Event, Message, State) ->
    ?LOG_WARNING("~p", [{self(), passive, Event, Message, State}]),
    {keep_state, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @TODO cleanup this part of the code.x
%% @end
%%--------------------------------------------------------------------
decoder(Message, #?MODULE{ payload = Payload, decoder = Decoder } = State) ->
    ?LOG_INFO("~p~n", [{info, Message, Payload, State}]),
    _ = insert(Message, State),
    NewPayload = <<Payload/bitstring, Message/bitstring>>,
    try berty_etf:decode(NewPayload, Decoder) of
        {ok, Decoded} -> 
            ?LOG_INFO("~p~n", [{debug, Decoded, NewPayload}]),
            NewState = State#?MODULE{ payload = NewPayload 
                                    , term = Decoded },
            {stop, normal, NewState};
        _ ->
            NewState = State#?MODULE{ payload = NewPayload
                                    , limit = State#?MODULE.limit+1
                                    },
            {keep_state, NewState}
    catch
        _:_ ->
            NewState = State#?MODULE{ payload = NewPayload
                                    , limit = State#?MODULE.limit+1
                                    },
            {keep_state, NewState}
    end.

insert(Message, #?MODULE{ store = Store } = _State) ->
    case ets:last(Store) of
        '$end_of_table' ->
            true = ets:insert(Store, {{0, erlang:system_time()}, Message});        
        {Id,_} ->
            true = ets:insert(Store, {{Id+1, erlang:system_time()}, Message})
    end.
                                     

