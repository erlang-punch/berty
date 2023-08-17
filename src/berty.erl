%%%===================================================================
%%% @copyright Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%%
%%% == Features ==
%%%
%%% <ul>
%%%  <li>Atoms conversion safety (already created, limitation, as binary, as list...)</li>
%%%  <li>Warnings message</li>
%%%  <li>Debug messages (partial)</li>
%%%  <li>Stats (todo)</li>
%%%  <li>Custom term interpretation (todo)</li>
%%%  <li>Specification (partial)</li>
%%%  <li>Unit testing (partial)</li>
%%%  <li>Property based testing (partial)</li>
%%% </ul>
%%%
%%% == Usage ==
%%%
%%% ```
%%% '''
%%%
%%% == Examples ==
%%%
%%% ```
%%% '''
%%%
%%% == Supported Terms ==
%%%
%%% This project has been created to offer a safe way to encode and
%%% decode External Term Format also called BERT for Binary ERlang
%%% Term). Erlang terms are not supported everywhere, and BERT only
%%% defines a small subset of them. The following table give you a
%%% quick view of which terms are enabled/disabled by default and
%%% their implementation state.
%%%
%%%
%%% ```
%%% | Terms              | Code |  Default | State                    |
%%% |---------------------|-----|----------|--------------------------|
%%% | ATOM_CACHE_REF      |  82 |  enabled | partial (unstable)
%%% | ATOM_EXT            | 100 |  enabled | implemented (deprecated)
%%% | ATOM_UTF8_EXT       | 118 |  enabled | partial (unstable)
%%% | BINARY_EXT          | 109 |  enabled | implemented
%%% | BIT_BINARY_EXT      |  77 |  enabled | implemented
%%% | EXPORT_EXT          | 113 | disabled | implemented (unstable)
%%% | FLOAT_EXT           |  99 |  enabled | partial
%%% | FUN_EXT             | 117 | disabled | todo (removed)
%%% | INTEGER_EXT         |  98 |  enabled | implemented
%%% | LARGE_BIG_EXT       | 111 |  enabled | implemented (slow or cursed)
%%% | LARGE_TUPLE_EXT     | 105 |  enabled | implemented
%%% | LIST_EXT            | 108 |  enabled | implemented
%%% | LOCAL_EXT           | 121 | disabled | todo
%%% | MAP_EXT             | 116 |  enabled | partial (unstable)
%%% | NEWER_REFERENCE_EXT |  90 | disabled | todo
%%% | NEW_FLOAT_EXT       |  70 |  enabled | partial (unstable)
%%% | NEW_FUN_EXT         | 112 | disabled | partial (cursed)
%%% | NEW_PID_EXT         |  88 | disabled | partial (unstable)
%%% | NEW_PORT_EXT        |  89 | disabled | partial (unstable)
%%% | NEW_REFERENCE_EXT   | 114 | disabled | todo
%%% | NIL_EXT             | 106 |  enabled | implemented
%%% | PID_EXT             | 103 | disabled | partial (unstable)
%%% | PORT_EXT            | 102 | disabled | partial (unstable)
%%% | REFERENCE_EXT       | 101 | disabled | todo (deprecated)
%%% | SMALL_ATOM_EXT      | 115 |  enabled | implemented (deprecated)
%%% | SMALL_ATOM_UTF8_EXT | 119 |  enabled | partial (unstable)
%%% | SMALL_BIG_EXT       | 110 |  enabled | implemented (slow or cursed)
%%% | SMALL_INTEGER_EXT   |  97 |  enabled | implemented
%%% | SMALL_TUPLE_EXT     | 104 |  enabled | implemented
%%% | STRING_EXT          | 107 |  enabled | implemented
%%% | V4_PORT_EXT         | 120 | disabled | partial (unstable)
%%% '''
%%%
%%% @end
%%%===================================================================
-module(berty).
-export([decode/1, decode/2]).
-export([encode/1, encode/2]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
decode(Data) -> berty_etf:decode(Data).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
decode(Data, Opts) -> berty_etf:decode(Data, Opts).
    

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
encode(Data) -> encode(Data, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
encode(Data, _Opts) -> {ok, erlang:term_to_binary(Data)}.
