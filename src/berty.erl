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
%%% | ATOM_CACHE_REF      |  82 |  enabled | todo
%%% | ATOM_EXT            | 100 |  enabled | implemented (deprecated)
%%% | ATOM_UTF8_EXT       | 118 |  enabled | partial (unstable)
%%% | BINARY_EXT          | 109 |  enabled | implemented
%%% | BIT_BINARY_EXT      |  77 |  enabled | implemented
%%% | EXPORT_EXT          | 113 | disabled | todo
%%% | FLOAT_EXT           |  99 |  enabled | partial
%%% | FUN_EXT             | 117 | disabled | todo (removed)
%%% | INTEGER_EXT         |  98 |  enabled | implemented
%%% | LARGE_BIG_EXT       | 111 |  enabled | implemented
%%% | LARGE_TUPLE_EXT     | 105 |  enabled | implemented
%%% | LIST_EXT            | 108 |  enabled | implemented
%%% | LOCAL_EXT           | 121 | disabled | todo
%%% | MAP_EXT             | 116 |  enabled | todo
%%% | NEWER_REFERENCE_EXT |  90 | disabled | todo
%%% | NEW_FLOAT_EXT       |  70 |  enabled | partial (unstable)
%%% | NEW_FUN_EXT         | 112 | disabled | todo
%%% | NEW_PID_EXT         |  88 | disabled | partial (unstable)
%%% | NEW_PORT_EXT        |  89 | disabled | partial (unstable)
%%% | NEW_REFERENCE_EXT   | 114 | disabled | todo
%%% | NIL_EXT             | 106 |  enabled | implemented
%%% | PID_EXT             | 103 | disabled | partial (unstable)
%%% | PORT_EXT            | 102 | disabled | partial (unstable)
%%% | REFERENCE_EXT       | 101 | disabled | todo (deprecated)
%%% | SMALL_ATOM_EXT      | 115 |  enabled | implemented (deprecated)
%%% | SMALL_ATOM_UTF8_EXT | 119 |  enabled | partial (unstable)
%%% | SMALL_BIG_EXT       | 110 |  enabled | implemented
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
-compile(export_all).
-include_lib("kernel/include/logger.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-record(state, { header = ""
               , version = 1
               , compressed = false
               , data = []
               , position = 0
               , stats = #{}
               , created_at = undefined
               , stopped_at = undefined
               }).

%---------------------------------------------------------------------
% term codes from https://www.erlang.org/doc/apps/erts/erl_ext_dist
%---------------------------------------------------------------------
-define(      ATOM_CACHE_REF,  82).
-define(            ATOM_EXT, 100).
-define(       ATOM_UTF8_EXT, 118).
-define(          BINARY_EXT, 109).
-define(      BIT_BINARY_EXT,  77).
-define(          EXPORT_EXT, 113).
-define(           FLOAT_EXT,  99).
-define(             FUN_EXT, 117).
-define(         INTEGER_EXT,  98).
-define(       LARGE_BIG_EXT, 111).
-define(     LARGE_TUPLE_EXT, 105).
-define(            LIST_EXT, 108).
-define(           LOCAL_EXT, 121).
-define(             MAP_EXT, 116).
-define( NEWER_REFERENCE_EXT,  90).
-define(       NEW_FLOAT_EXT,  70).
-define(         NEW_FUN_EXT, 112).
-define(         NEW_PID_EXT,  88).
-define(        NEW_PORT_EXT,  89).
-define(   NEW_REFERENCE_EXT, 114).
-define(             NIL_EXT, 106).
-define(             PID_EXT, 103).
-define(            PORT_EXT, 102).
-define(       REFERENCE_EXT, 101).
-define(      SMALL_ATOM_EXT, 115).
-define( SMALL_ATOM_UTF8_EXT, 119).
-define(       SMALL_BIG_EXT, 110).
-define(   SMALL_INTEGER_EXT,  97).
-define(     SMALL_TUPLE_EXT, 104).
-define(          STRING_EXT, 107).
-define(         V4_PORT_EXT, 120).

%---------------------------------------------------------------------
% helper function to map code and code name
%---------------------------------------------------------------------
-define(CODE(Code,Name), code(Code) -> Name; code(Name) -> Code).
?CODE( 70, new_float_ext);
?CODE( 77, bit_binary_ext);
?CODE( 82, atom_cache_ref);
?CODE( 88, new_pid_ext);
?CODE( 89, new_port_ext);
?CODE( 90, newer_reference_ext);
?CODE( 97, small_integer_ext);
?CODE( 98, integer_ext);
?CODE( 99, float_ext);
?CODE(100, atom_ext);
?CODE(101, reference_ext);
?CODE(102, port_ext);
?CODE(103, pid_ext);
?CODE(104, small_tuple_ext);
?CODE(105, large_tuple_ext);
?CODE(106, nil_ext);
?CODE(107, string_ext);
?CODE(108, list_ext);
?CODE(109, binary_ext);
?CODE(110, small_big_ext);
?CODE(111, large_big_ext);
?CODE(112, new_fun_ext);
?CODE(113, export_ext);
?CODE(114, new_reference_ext);
?CODE(115, small_atom_ext);
?CODE(116, map_ext);
?CODE(117, fun_ext);
?CODE(118, atom_utf8_ext);
?CODE(119, small_atom_utf8_ext);
?CODE(120, v4_port_ext);
?CODE(121, local_ext);
code(Elsewise) -> Elsewise.

%%--------------------------------------------------------------------
%% @doc
%% @see decode/2
%% @end
%%--------------------------------------------------------------------
-spec decode(Binary) -> Return when
      Binary :: binary(),
      Return :: {ok, term()}.

decode(Data) ->
    decode(Data, #{}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, term()}.

decode(Data, Opts) ->
    case decode_header(Data, Opts, #state{}) of
        {ok, Terms, <<>>} ->
            {ok, Terms};
        Elsewise -> Elsewise
    end.

decode_test() ->
    [?assertEqual({ok, 1}, decode(term_to_binary(1)))
    ,?assertEqual({ok, 2}, decode(term_to_binary(2)))
    ,?assertEqual({ok, -1}, decode(term_to_binary(-1)))
    ,proper:quickcheck(decode_integer_ext_properties())
    ,proper:quickcheck(decode_small_integer_ext_properties())
    ].

%%--------------------------------------------------------------------
%% @hidden
 %% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode_header(Binary, Opts, Buffer) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Buffer :: #state{},
      Return :: {ok, term()}.

decode_header(<<131, Rest/binary>>, Opts, Buffer) ->
    NewBuffer = Buffer#state{ position = Buffer#state.position+1 },
    decode_terms(Rest, Opts, NewBuffer).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode_terms(Binary, Opts, Buffer) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Buffer :: #state{},
      Return :: {ok, term(), binary()}.

decode_terms(<<>>, _Opts, #state{ data = Data} = _Buffer) ->
    {ok, Data, <<>>};
decode_terms(Rest, _Opts, #state{ data = Data } = _Buffer)
  when is_number(Data) orelse is_atom(Data) ->
    {ok, Data, Rest};

%---------------------------------------------------------------------
% integers support
%---------------------------------------------------------------------
decode_terms(<<?SMALL_INTEGER_EXT, _Rest/binary>>, #{ small_integer_ext := disabled }, _Buffer) ->
    {error, {small_integer_ext, disabled}};
decode_terms(<<?SMALL_INTEGER_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _Integer, _Rest2} = decode_small_integer_ext(Rest, Opts);

decode_terms(<<?INTEGER_EXT, _Rest/binary>>, #{ integer_ext := disable}, _Buffer) ->
    {error, {integer_ext, disabled}};
decode_terms(<<?INTEGER_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _Integer, _Rest2} = decode_integer_ext(Rest, Opts);

%---------------------------------------------------------------------
% floats support
%---------------------------------------------------------------------
decode_terms(<<?FLOAT_EXT, _Rest/binary>>, #{ float_ext := disabled }, _Buffer) ->
    {error, {float_ext, disabled}};
decode_terms(<<?FLOAT_EXT, Rest/binary>>, #{ minor_version := 0 } = Opts, _Buffer) ->
    {ok, _Float, _Rest2} = decode_float_ext(Rest, Opts);

%---------------------------------------------------------------------
% atoms support
%---------------------------------------------------------------------
decode_terms(<<?ATOM_EXT, _Rest/binary>>, #{ atom_ext := disabled }, _Buffer) ->
    {error, {atom_ext, disabled}};
decode_terms(<<?ATOM_EXT, Rest/binary>>, Opts, _Buffer) ->
    ?LOG_WARNING("ATOM_EXT is deprecated", []),
    {ok, _Atom, _Rest2} = decode_atom_ext(Rest, Opts);

decode_terms(<<?SMALL_ATOM_EXT, _Rest/binary>>, #{ small_atom_ext := disabled }, _Buffer) ->
    {error, {small_atom_ext, disabled}};
decode_terms(<<?SMALL_ATOM_EXT, Rest/binary>>, Opts, _Buffer) ->
    ?LOG_WARNING("SMALL_ATOM_EXT is deprecated", []),
    {ok, _Atom, _Rest2} = decode_small_atom_ext(Rest, Opts);

decode_terms(<<?ATOM_UTF8_EXT, _Rest/binary>>, #{ atom_utf8_ext := disabled }, _Buffer) ->
    {error, {atom_utf8_ext, disabled}};
decode_terms(<<?ATOM_UTF8_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _Atom, _Rest2} = decode_atom_utf8_ext(Rest, Opts);

decode_terms(<<?SMALL_ATOM_UTF8_EXT, _Rest/binary>>, #{ small_atom_utf8_ext := disabled }, _Buffer) ->
    {error, {small_atom_utf8_ext, disabled}};
decode_terms(<<?SMALL_ATOM_UTF8_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _Atom, _Rest2} = decode_small_atom_utf8_ext(Rest, Opts);

%---------------------------------------------------------------------
% tuple support
%---------------------------------------------------------------------
decode_terms(<<?SMALL_TUPLE_EXT, _Rest/binary>>, #{ small_tuple_ext := disabled }, _Buffer) ->
    {error, {small_tuple_ext, disabled}};
decode_terms(<<?SMALL_TUPLE_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _Tuple, _Rest2} = decode_small_tuple_ext(Rest, Opts);

decode_terms(<<?LARGE_TUPLE_EXT, _Rest/binary>>, #{ large_tuple_ext := disabled }, _Buffer) ->
    {error, {large_tuple_ext, disabled}};
decode_terms(<<?LARGE_TUPLE_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _Tuple, _Rest2} = decode_large_tuple_ext(Rest, Opts);

%---------------------------------------------------------------------
% list and string support
%---------------------------------------------------------------------
decode_terms(<<?NIL_EXT, _Rest/binary>>, #{ string_ext := disabled }, _Buffer) ->
    {error, {string_ext, disabled}};
decode_terms(<<?NIL_EXT, _Rest/binary>>, #{ list_ext := disabled }, _Buffer) ->
    {error, {list_ext, disabled}};
decode_terms(<<?NIL_EXT, Rest/binary>>, _Opts, _Buffer) ->
    {ok, [], Rest};

decode_terms(<<?STRING_EXT, _Rest/binary>>, #{ string_ext := disabled }, _Buffer) ->
    {error, {string_ext, disabled}};
decode_terms(<<?STRING_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _String, _Rest2} = decode_string_ext(Rest, Opts);

decode_terms(<<?LIST_EXT, _Rest/binary>>, #{ list_ext := disabled }, _Buffer) ->
    {error, {list_ext, disabled}};
decode_terms(<<?LIST_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _List, _Rest2} = decode_list_ext(Rest, Opts);

%---------------------------------------------------------------------
% binary and bitstring
%---------------------------------------------------------------------
decode_terms(<<?BINARY_EXT, _Rest/binary>>, #{ binary_ext := disabled }, _Buffer) ->
    {error, {binary_ext, disabeld}};
decode_terms(<<?BINARY_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _Binary, _Rest2} = decode_binary_ext(Rest, Opts);

decode_terms(<<?BIT_BINARY_EXT, _Rest/binary>>, #{ bit_binary_ext := disabled }, _Buffer) ->
    {error, {bit_binary_ext, disabled}};
decode_terms(<<?BIT_BINARY_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _Bitstring, _Rest2} = decode_bit_binary_ext(Rest, Opts);

%---------------------------------------------------------------------
% port
%---------------------------------------------------------------------
decode_terms(<<?PORT_EXT, Rest/binary>>, #{ port_ext := enabled } = Opts, _Buffer) ->
    {ok, _Port, _Rest2} = decode_port_ext(Rest, Opts);

decode_terms(<<?NEW_PORT_EXT, Rest/binary>>, #{ new_port_ext := enabled } = Opts, _Buffer) ->
    {ok, _Port, _Rest2} = decode_new_port_ext(Rest, Opts);

decode_terms(<<?V4_PORT_EXT, Rest/binary>>, #{ v4_port_ext := enabled } = Opts, _Buffer) ->
    {ok, _Port, _Rest2} = decode_v4_port_ext(Rest, Opts);

%---------------------------------------------------------------------
% pid
%---------------------------------------------------------------------
decode_terms(<<?PID_EXT, Rest/binary>>, #{ pid_ext := enabled } = Opts, _Buffer) ->
    {ok, _Pid, _Rest2} = decode_pid_ext(Rest, Opts);

decode_terms(<<?NEW_PID_EXT, Rest/binary>>, #{ new_pid_ext := enabled } = Opts, _Buffer) ->
    {ok, _Pid, _Rest2} = decode_new_pid_ext(Rest, Opts);

%---------------------------------------------------------------------
% big num
%---------------------------------------------------------------------
decode_terms(<<?SMALL_BIG_EXT, _Rest/binary>>, #{ small_big_ext := disabled }, _Buffer) ->
    {error, {small_big_ext, disabled}};
decode_terms(<<?SMALL_BIG_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _Bignum, _Rest2} = decode_small_big_ext(Rest, Opts);

decode_terms(<<?LARGE_BIG_EXT, _Rest/binary>>, #{ large_big_ext := disabled }, _Buffer) ->
    {error, {large_big_ext, disabled}};
decode_terms(<<?LARGE_BIG_EXT, Rest/binary>>, Opts, _Buffer) ->
    {ok, _Bignum, _Rest2} = decode_large_big_ext(Rest, Opts);

%---------------------------------------------------------------------
% lambdas
%---------------------------------------------------------------------
decode_terms(<<?EXPORT_EXT, Rest/binary>>, #{ export_ext := enabled } = Opts, _Buffer) ->
    {ok, _Fun, _Rest2} = decode_export_ext(Rest, Opts);

%---------------------------------------------------------------------
% wildcard pattern
%---------------------------------------------------------------------
decode_terms(<<Code, _/binary>>, _Opts, _Buffer) ->
    {error, {unsupported, code(Code)}}.

%%--------------------------------------------------------------------
%% @hidden
%%
%% @doc atoms are a big problem for BERT and ETF, even more if the
%% application is communicating with unknown devices. This function
%% extend the binary_to_term/1 built-in function by defining how to
%% deal with atoms.
%%
%% == Examples ==
%%
%% The default behavior is to convert all atoms as binary, their raw
%% form from payload.
%%
%% ```
%% berty:decode(<<131,100,0,2,111,107>>, #{}).
%% {ok, <<"ok">>}
%%
%% berty:decode(<<131,100,0,2,111,107>>, #{ atoms => as_binary }).
%% {ok,<<"ok">>}
%% '''
%%
%% This function support the unconditional creation of atoms, like
%% binary_to_term/1 built-in function, but this feature can kill a
%% node.
%%
%% ```
%% berty:decode(<<131,100,0,2,111,107>>, #{ atoms => create }).
%% {ok,ok}
%% '''
%%
%% Creates atoms with a limit based on system_info/1 built-in
%% function. The formula used is quite simple:
%% AtomCounter/AtomSystemLimit, if Limit is greater than this formula,
%% the atom is returned as binary.
%%
%% ```
%% berty:decode(<<131,100,0,2,111,107>>, #{ atoms => {create, 0.001} }).
%% {ok, <<"ok">>}
%%
%% berty:decode(<<131,100,0,2,111,107>>, #{ atoms => {create, 0.001, warning} }).
%% =WARNING REPORT==== 10-Aug-2023::16:37:44.960832 ===
%% Atom limits 0.001 exceed (0.01691436767578125)
%% {ok,<<"ok">>}
%% '''
%%
%% Creates atoms only if they are already present on the node.
%%
%% ```
%% berty:decode(<<131,100,0,2,111,107>>, #{ atoms => existing }).
%% {ok, ok}
%%
%% berty:decode(<<131,100,0,2,111,104>>, #{ atoms => existing }).
%% {ok, <<"oh">>}
%% '''
%%
%% Creates atoms as string (list of integers).
%%
%% ```
%% berty:decode(<<131,100,0,2,111,107>>, #{ atoms => as_string }).
%% {ok, "ok"}
%% '''
%%
%% Finally, a simple atom whitelist. If an atom (defined as binary) is
%% not present in the list, it is returned as binary, else, it is
%% converted as atoms.
%%
%% ```
%% Binary = [1,2,3,[4,5,6,{1,2,3}, "test", atom]].
%%
%% % with an empty whitelist
%% {ok,[1,2,3,[4,5,6,{1,2,3},"test",<<"atom">>]],<<>>}
%%   = berty:decode(Binary, #{ atoms => {whitelist, []}}).
%% % Warning: ATOM_EXT is deprecated
%% % Warning: Atom <<"atom">> not in whitelist
%%
%% {ok,[1,2,3,[4,5,6,{1,2,3},"test",atom]],<<>>}
%%   = berty:decode(Binary, #{ atoms => {whitelist, [<<"atom">>]}}).
%% % Warning: ATOM_EXT is deprecated
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec decode_atoms(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: #{ atoms := AtomOpts },
      AtomOpts :: create
                | {create, Limit}
                | {create, Limit, warning}
                | existing
                | as_string
                | term(),
      Limit :: float(),
      Return :: binary() | string() | atom().

% @TODO add utf8 support for atoms
decode_atoms(Binary, #{ atoms := {whitelist, Whitelist} })
  when is_list(Whitelist) ->
    case lists:member(Binary, Whitelist) of
        true ->
            erlang:binary_to_atom(Binary);
        false ->
            ?LOG_WARNING("Atom ~p not in whitelist", [Binary]),
            Binary
    end;
decode_atoms(Binary, #{ atoms := create }) ->
    erlang:binary_to_atom(Binary);
decode_atoms(Binary, #{ atoms := {create, Limit} })
  when is_float(Limit) andalso
       Limit >= 0 andalso
       Limit =< 1 ->
    case erlang:system_info(atom_count)/erlang:system_info(atom_limit) of
        X when X >= Limit -> Binary;
        _ -> erlang:binary_to_atom(Binary)
    end;
decode_atoms(Binary, #{ atoms := {create, Limit, warning} })
  when is_float(Limit) andalso Limit >= 0 andalso Limit =< 1 ->
    case erlang:system_info(atom_count)/erlang:system_info(atom_limit) of
        X when X >= Limit ->
            ?LOG_WARNING("Atom limits ~p exceed (~p)", [Limit, X]),
            Binary;
        _ -> erlang:binary_to_atom(Binary)
    end;
decode_atoms(Binary, #{ atoms := as_string }) ->
    erlang:binary_to_list(Binary);
decode_atoms(Binary, #{ atoms := existing }) ->
    try erlang:binary_to_existing_atom(Binary)
    catch
        _:_ -> Binary
    end;
decode_atoms(Binary, _) ->
    Binary.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#small_integer_ext
%% @end
%%--------------------------------------------------------------------
-spec decode_small_integer_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, integer(), binary()}.

decode_small_integer_ext(<<Integer, Rest/binary>>, Opts) ->
    ?LOG_DEBUG("~p", [{{decode_small_integer_ext, code(small_integer_ext)}, [Integer], Opts}]),
    {ok, Integer, Rest}.

decode_small_integer_ext_test() ->
    [?assertEqual({ok, 0, <<>>}, decode_small_integer_ext(<<0>>, #{}))
    ,?assertEqual({ok, 1, <<>>}, decode_small_integer_ext(<<1>>, #{}))
    ,?assertEqual({ok, 255, <<>>}, decode_small_integer_ext(<<255>>, #{}))
    ,?assertEqual({ok, 255, <<0>>}, decode_small_integer_ext(<<255,0>>, #{}))
    ].

decode_small_integer_ext_properties() ->
    Fun = fun(Binary) ->
                  <<Expected/integer, _/binary>> = Binary,
                  {ok, Result, _} = decode_small_integer_ext(Binary, #{}),
                  Result =:= Expected
          end,
    ?FORALL(Binary, binary(4), Fun(Binary)).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#integer_ext
%% @end
%%--------------------------------------------------------------------
-spec decode_integer_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, integer(), binary()}.

decode_integer_ext(<<Integer:32/signed-integer, Rest/binary>>, Opts) ->
    ?LOG_DEBUG("~p", [{{decode_integer_ext, code(integer_ext)}, [Integer], Opts}]),
    {ok, Integer, Rest}.

decode_integer_ext_test() ->
    [?assertEqual({ok, 0, <<>>}, decode_small_integer_ext(<<0,0,0,0>>, #{}))
    ,?assertEqual({ok, 255, <<>>}, decode_small_integer_ext(<<0,0,0,255>>, #{}))
    ,?assertEqual({ok, 65280, <<>>}, decode_small_integer_ext(<<0,0,255,0>>, #{}))
    ,?assertEqual({ok, 16711680, <<>>}, decode_small_integer_ext(<<0,255,0,0>>, #{}))
    ,?assertEqual({ok, -16777216, <<>>}, decode_small_integer_ext(<<255,0,0,0>>, #{}))
    ].

decode_integer_ext_properties() ->
    Fun = fun(Binary) ->
                  <<Expected:32/integer, _/binary>> = Binary,
                  {ok, Result, _} = decode_integer_ext(Binary, #{}),
                  Expected =:= Result
          end,
    ?FORALL(Binary, binary(4), Fun(Binary)).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#float_ext
%% @end
%%--------------------------------------------------------------------
-spec decode_float_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, float(), binary()}.

decode_float_ext(<<Float:31/binary, Rest/binary>>, Opts) ->
    NewFloat = binary_to_float(Float),
    ?LOG_DEBUG("~p", [{{decode_float_ext, code(float_ext)}, [Float], Opts}]),
    {ok, NewFloat, Rest}.

decode_float_ext_test() ->
    E = #{ minor_version => 0 },
    Opts = [{minor_version, 0}],
    [?assertEqual({ok, 1.0, <<>>}, decode_float_ext(term_to_binary(1.0, Opts), E))
    ,?assertEqual({ok, 2.0, <<>>}, decode_float_ext(term_to_binary(2.0, Opts), E))
    ,?assertEqual({ok, 1.0e10, <<>>}, decode_float_ext(term_to_binary(1.0e10, Opts), E))
    ].

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#new_float_ext
%% see: https://en.wikipedia.org/wiki/Double-precision_floating-point_format
%% @todo to fix, the implementation is not working correctly.
%% @end
%%--------------------------------------------------------------------
-spec decode_new_float_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, float(), binary()}.

decode_new_float_ext(<<_Float:64/bitstring, _Rest/binary>>, _Opts) ->
    %% ExponentBias = 1023,
    %% <<Sign:1, Exponent:11, Fraction:52>> = Float,
    %% Value = math:pow(2, Exponent-ExponentBias) * (1/Fraction),
    %% case Sign of
    %%     0 -> {ok, Value, Rest};
    %%     1 -> {ok, -Value, Rest}
    %% end,
    {error, {not_supported, new_float_ext}}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#atom_utf8_ext
%% @end
%%--------------------------------------------------------------------
-spec decode_atom_utf8_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, Atom, binary()},
      Atom :: atom() | string() | binary().

decode_atom_utf8_ext(<<Length:16/unsigned-integer, Rest/binary>>, Opts) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = decode_atoms(Atom, Opts),
    ?LOG_DEBUG("~p", [{{decode_atom_utf8_ext, code(atom_utf8_ext)}, [Length, Atom, NewAtom], Opts}]),
    {ok, NewAtom, Rest2}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#small_atom_utf8_ext
%% @end
%%--------------------------------------------------------------------
-spec decode_small_atom_utf8_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, Atom, binary()},
      Atom :: atom() | string() | binary().

decode_small_atom_utf8_ext(<<Length/unsigned-integer, Rest/binary>>, Opts) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = decode_atoms(Atom, Opts),
    ?LOG_DEBUG("~p", [{{decode_small_atom_utf8_ext, code(small_atom_utf8_ext)}, [Length, Atom, NewAtom], Opts}]),
    {ok, NewAtom, Rest2}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#atom_ext--deprecated-
%% @end
%%--------------------------------------------------------------------
-spec decode_atom_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, Atom, binary()},
      Atom :: atom() | string() | binary().

decode_atom_ext(<<Length:16/unsigned-integer, Rest/binary>>, Opts) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = decode_atoms(Atom, Opts),
    ?LOG_DEBUG("~p", [{{decode_atom_ext, code(atom_ext)}, [Length, Atom, NewAtom], Opts}]),
    {ok, NewAtom, Rest2}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#small_atom_ext--deprecated-
%% @end
%%--------------------------------------------------------------------
-spec decode_small_atom_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, Atom, binary()},
      Atom :: atom() | string() | binary().

decode_small_atom_ext(<<115, Length:8/unsigned-integer, Rest/binary>>, Opts) ->
    <<Atom:Length, Rest2/binary>> = Rest,
    NewAtom = decode_atoms(Atom, Opts),
    ?LOG_DEBUG("~p", [{{decode_small_atom_ext, code(small_atom_ext)}, [Length, Atom, NewAtom], Opts}]),
    {ok, NewAtom, Rest2}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc decode small_tuple_ext data.
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#small_tuple_ext
%% @end
%%--------------------------------------------------------------------
-spec decode_small_tuple_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, tuple(), binary()}.

decode_small_tuple_ext(<<0, Rest/binary>>, _Opts) ->
    {ok, {}, Rest};
decode_small_tuple_ext(<<Arity/unsigned-integer, Rest/binary>>, Opts) ->
    decode_small_tuple_ext2(Arity, Rest, Opts, []).

decode_small_tuple_ext2(0, Rest, Opts, Buffer) ->
    Tuple = erlang:list_to_tuple(lists:reverse(Buffer)),
    ?LOG_DEBUG("~p", [{{decode_small_tuple_ext2, code(small_tuple_ext)}, [0, Tuple], Opts}]),
    {ok, Tuple, Rest};
decode_small_tuple_ext2(Arity, Rest, Opts, Buffer) ->
    {ok, Term, Rest2} = decode_terms(Rest, Opts, #state{}),
    ?LOG_DEBUG("~p", [{{decode_small_tuple_ext2, code(small_tuple_ext)}, [Arity, Term], Opts}]),
    decode_small_tuple_ext2(Arity-1, Rest2, Opts, [Term|Buffer]).

%%--------------------------------------------------------------------
%% @hidden
%% @doc decode large_tuple_ext data and all elements in it.
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#large_tuple_ext
%% @end
%%--------------------------------------------------------------------
-spec decode_large_tuple_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, tuple(), binary()}.

decode_large_tuple_ext(<<0:32, Rest/binary>>, _Opts) ->
    {ok, {}, Rest};
decode_large_tuple_ext(<<Arity:32/unsigned-integer, Rest/binary>>, Opts) ->
    decode_large_tuple_ext2(Arity, Rest, Opts, []).

decode_large_tuple_ext2(0, Rest, Opts, Buffer) ->
    Tuple = erlang:list_to_tuple(lists:reverse(Buffer)),
    ?LOG_DEBUG("~p", [{{decode_large_tuple_ext2, code(large_tuple_ext)}, [0, Tuple], Opts}]),
    {ok, Tuple, Rest};
decode_large_tuple_ext2(Arity, Rest, Opts, Buffer) ->
    {ok, Term, Rest2} = decode_terms(Rest, Opts, #state{}),
    ?LOG_DEBUG("~p", [{{decode_large_tuple_ext2, code(large_tuple_ext)}, [Arity, Term], Opts}]),
    decode_large_tuple_ext2(Arity-1, Rest2, Opts, [Term|Buffer]).

%%--------------------------------------------------------------------
%% @hidden
%% @doc decode a serialized string_ext data.
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#string_ext
%% @end
%%--------------------------------------------------------------------
decode_string_ext(<<Length:16/unsigned-integer, Characters/binary>>, Opts) ->
    decode_string_ext2(Length, Characters, Opts, "").

decode_string_ext2(0, Rest, Opts, Buffer) ->
    String = lists:reverse(Buffer),
    ?LOG_DEBUG("~p", [{{decode_string_ext2, code(string_ext)}, String, Opts}]),
    {ok, String, Rest};
decode_string_ext2(Length, <<Character:8/unsigned-integer, Rest/binary>>, Opts, Buffer) ->
    decode_string_ext2(Length-1, Rest, Opts, [Character|Buffer]).

%%--------------------------------------------------------------------
%% @hidden
%% @doc decode a serialized list_ext with all elements in it.
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#list_ext
%% @end
%%--------------------------------------------------------------------
decode_list_ext(<<Length:32/unsigned-integer, Elements/binary>>, Opts) ->
    {ok, List, <<106, Rest/binary>>} = decode_list_ext2(Length, Elements, Opts, []),
    {ok, List, Rest}.

decode_list_ext2(0, Rest, Opts, Buffer) ->
    List = lists:reverse(Buffer),
    ?LOG_DEBUG("~p", [{{decode_list_ext2, code(list_ext)}, List, Opts}]),
    {ok, List, Rest};
decode_list_ext2(Length, Elements, Opts, Buffer) ->
    {ok, Term, Rest} = decode_terms(Elements, Opts, #state{}),
    decode_list_ext2(Length-1, Rest, Opts, [Term|Buffer]).

%%--------------------------------------------------------------------
%% @hidden
%% @doc decode a serialized binary_ext data.
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#binary_ext
%% @end
%%--------------------------------------------------------------------
decode_binary_ext(<<Length:32/unsigned-integer, Rest/binary>>, Opts) ->
    <<Binary:Length/binary, Rest2/binary>> = Rest,
    ?LOG_DEBUG("~p", [{{decode_binary_ext, code(binary_ext)}, [Length, Binary], Opts}]),
    {ok, Binary, Rest2}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc decode a serialized bit_binary_ext data.
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#bit_binary_ext
%% @end
%%--------------------------------------------------------------------
decode_bit_binary_ext(<<Length:32/unsigned-integer, Bits:8, Rest/binary>>, Opts) ->
    <<Binary:(Length-1)/binary, Byte:1/binary, Rest2/binary>> = Rest,
    ?LOG_DEBUG("~p", [{{decode_bit_binary_ext, code(bit_binary_ext)}, [Length, Bits, Binary, Byte], Opts}]),
    {ok, <<Binary/binary, Byte:Bits/bitstring>>, Rest2}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc decode a serialized port_ext data. Avoid using it in prod.
%%
%% This function is disabled by default. It has been added simply to
%% offer a complete ETF implementation.
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#port_ext
%% @end
%%--------------------------------------------------------------------
decode_port_ext(Binary, Opts) ->
    {ok, Atom, Rest} = decode_terms(Binary, Opts#{ atoms => create }, #state{}),
    case is_atom(Atom) of
        true ->
            <<ID:32/unsigned-integer, Creation:1/binary, Rest2/binary>> = Rest,
            StringPort = lists:flatten(io_lib:format("#Port<~B.~B>", [Creation,ID])),
            {tofix, list_to_port(StringPort), Rest2};
       false ->
            {error, {not_atom, Atom}}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc decode a serialized new_port_ext data. Avoid using it in prod.
%%
%% This function is disabled by default. It has been added simply to
%% offer a complete ETF implementation.
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#new_port_ext
%% @end
%%--------------------------------------------------------------------
decode_new_port_ext(Binary, Opts) ->
    {ok, Atom, Rest} = decode_terms(Binary, Opts#{ atoms => create }, #state{}),
    case is_atom(Atom) of
        true ->
            <<ID:32/unsigned-integer, Creation:32/unsigned-integer, Rest2/binary>> = Rest,
            StringPort = lists:flatten(io_lib:format("#Port<~B.~B>", [Creation,ID])),
            {tofix, list_to_port(StringPort), Rest2};
       false ->
            {error, {not_atom, Atom}}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc decode a serialized decode_v4_port_ext data. Avoid using it in
%% prod.
%%
%% This function is disabled by default. It has been added simply to
%% offer a complete ETF implementation.
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#v4_port_ext
%% @end
%%--------------------------------------------------------------------
decode_v4_port_ext(Binary, Opts) ->
    {ok, Atom, Rest} = decode_terms(Binary, Opts#{ atoms => create }, #state{}),
    case is_atom(Atom) of
        true ->
            <<ID:64/unsigned-integer, Creation:32/unsigned-integer, Rest2/binary>> = Rest,
            StringPort = lists:flatten(io_lib:format("#Port<~B.~B>", [Creation,ID])),
            {tofix, list_to_port(StringPort), Rest2};
       false ->
            {error, {not_atom, Atom}}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc decode a serialized pid_ext data. Avoid using it in prod.
%%
%% This function is disabled by default. It has been added simply to
%% offer a complete ETF implementation.
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#pid_ext
%% @end
%%--------------------------------------------------------------------
decode_pid_ext(Binary, Opts) ->
    {ok, NodeName, Rest} = decode_terms(Binary, Opts#{ atoms => create }, #state{}),
    case is_atom(NodeName) of
        true ->
            <<ID:32/unsigned-integer, Serial:32/unsigned-integer
             ,Creation:8/unsigned-integer, Rest2/binary>> = Rest,
            Pid = c:pid(Creation, ID, Serial),
            {ok, Pid, Rest2};
       false ->
            {error, {not_atom, NodeName}}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc decode a serialized new_pid_ext data. Avoid using it in prod.
%%
%% This function is disabled by default. It has been added simply to
%% offer a complete ETF implementation.
%%
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#new_pid_ext
%% @end
%%--------------------------------------------------------------------
decode_new_pid_ext(Binary, Opts) ->
    {ok, NodeName, Rest} = decode_terms(Binary, Opts#{ atoms => create }, #state{}),
    case is_atom(NodeName) of
        true ->
            <<ID:32/unsigned-integer, Serial:32/unsigned-integer
             ,Creation:32/unsigned-integer, Rest2/binary>> = Rest,
            Pid = c:pid(Creation, ID, Serial),
            {ok, Pid, Rest2};
       false ->
            {error, {not_atom, NodeName}}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_export_ext(Binary, Opts) ->
    {ok, Module, Rest1} = decode_terms(Binary, Opts#{ atoms => create }, #state{}),
    {ok, Function, Rest2} = decode_terms(Rest1, Opts#{ atoms => create }, #state{}),
    {ok, Arity, Rest3} = decode_terms(Rest2, Opts, #state{}),
    case {is_atom(Module), is_atom(Function), is_integer(Arity)} of
        {true, true, true} ->
            {ok, fun Module:Function/Arity, Rest3};
        {false, true, true} ->
            {error, {not_atom, Module}};
        {true, false, true} ->
            {error, {not_atom, Function}};
        {true, true, false} ->
            {error, {not_integer, Arity}};
        Error ->
            {error, {badterms, Error}}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#small_big_ext
%% @end
%%--------------------------------------------------------------------
decode_small_big_ext(<<Size:8/unsigned-integer, Sign:8/unsigned-integer, Rest/binary>>, _Opts) ->
    {ok, Bignum, Rest2} = decode_big_ext(Size, 0, Rest, 0),
    case Sign of
        0 -> {ok, +Bignum, Rest2};
        1 -> {ok, -Bignum, Rest2}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% see: https://www.erlang.org/doc/apps/erts/erl_ext_dist#large_big_ext
%% @end
%%--------------------------------------------------------------------
decode_large_big_ext(<<Size:32/unsigned-integer, Sign:8/unsigned-integer, Rest/binary>>, _Opts) ->
    {ok, Bignum, Rest2} = decode_big_ext(Size, 0, Rest, 0),
    case Sign of
        0 -> {ok, +Bignum, Rest2};
        1 -> {ok, -Bignum, Rest2}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc main function to decode big integer.
%% @end
%%--------------------------------------------------------------------
decode_big_ext(Size, Size, Rest, Bignum) ->
    {ok, Bignum, Rest};
decode_big_ext(Size, Counter, <<D:8/unsigned-integer, Rest/binary>>, Bignum) ->
    decode_big_ext(Size, Counter+1, Rest, Bignum + (D*pow(256, Counter))).

%%--------------------------------------------------------------------
%% @hidden
%% @doc quick and dirty pow implementation. It's slow but it works.
%% @end
%%--------------------------------------------------------------------
pow(X, Y) -> pow(X, Y, 1).
pow(_, 0, R) -> R;
pow(X, Y, R) -> pow(X, Y-1, R*X).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
encode(Data) ->
    encode(Data, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
encode(Data, _Opts) ->
    {ok, erlang:term_to_binary(Data)}.
