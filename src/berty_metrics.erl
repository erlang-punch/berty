%%%===================================================================
%%% @copyright Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%%
%%% collected:
%%%
%%% <ul>
%%% <li>sum of decoded ETF</li>
%%% <li>complete ETF size</li>
%%% <li>final result</li>
%%% <li>type</li>
%%% <li>terms type</li>
%%% <li>decoder mode</li>
%%% <li>min decoding time</li>
%%% <li>average decoding time</li>
%%% <li>max decoding time</li>
%%% </ul>
%%%
%%% ```
%%% | Terms                 | Stats collected         |
%%% |-----------------------|-------------------------|
%%% | `ATOM_CACHE_REF`      | count
%%% | `ATOM_EXT`            | count, size, repetition
%%% | `ATOM_UTF8_EXT`       | count, size, repetition
%%% | `BINARY_EXT`          | count, size
%%% | `BIT_BINARY_EXT`      | count, size
%%% | `EXPORT_EXT`          | count, size 
%%% | `FLOAT_EXT`           | count
%%% | `FUN_EXT`             | counter, size, arity
%%% | `INTEGER_EXT`         | count
%%% | `LARGE_BIG_EXT`       | count, size
%%% | `LARGE_TUPLE_EXT`     | count, arity
%%% | `LIST_EXT`            | count, length
%%% | `LOCAL_EXT`           | count
%%% | `MAP_EXT`             | count, pair
%%% | `NEWER_REFERENCE_EXT` | count, size
%%% | `NEW_FLOAT_EXT`       | count
%%% | `NEW_FUN_EXT`         | count, size
%%% | `NEW_PID_EXT`         | count
%%% | `NEW_PORT_EXT`        | count
%%% | `NEW_REFERENCE_EXT`   | count
%%% | `NIL_EXT`             | count
%%% | `PID_EXT`             | count
%%% | `PORT_EXT`            | count
%%% | `REFERENCE_EXT`       | count
%%% | `SMALL_ATOM_EXT`      | count, size, repetition
%%% | `SMALL_ATOM_UTF8_EXT` | count, size, repetition
%%% | `SMALL_BIG_EXT`       | count, size
%%% | `SMALL_INTEGER_EXT`   | count
%%% | `SMALL_TUPLE_EXT`     | count, arity
%%% | `STRING_EXT`          | count, length
%%% | `V4_PORT_EXT`         | count
%%% '''
%%%
%%% @end
%%%===================================================================
-module(berty_metrics).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([push/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push(Data) ->
    gen_statem:cast(?MODULE, {push, Data}).

init(_Args) ->
    State = ets:new(?MODULE, []),
    {ok, State}.

handle_call(_Message, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%%
%% ```
%% {push, {atom_ext, 
%% '''
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Messgae, State) ->
    {noreply, State}.


