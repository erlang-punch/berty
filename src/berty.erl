%%%===================================================================
%%% @doc
%%%
%%% == Usage ==
%%%
%%% == Examples ==
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
               }).

%---------------------------------------------------------------------
% term codes from https://www.erlang.org/doc/apps/erts/erl_ext_dist
%---------------------------------------------------------------------
-define(     ATOM_CACHE_REF,  82).
-define(           ATOM_EXT, 100). % deprecated
-define(      ATOM_UTF8_EXT, 118).
-define(         BINARY_EXT, 109).
-define(     BIT_BINARY_EXT,  77).
-define(         EXPORT_EXT, 113).
-define(          FLOAT_EXT,  99).
-define(            FUN_EXT, 117). % removed
-define(        INTEGER_EXT,  98).
-define(      LARGE_BIG_EXT, 111).
-define(    LARGE_TUPLE_EXT, 105).
-define(           LIST_EXT, 108).
-define(          LOCAL_EXT, 121).
-define(            MAP_EXT, 116).
-define(NEWER_REFERENCE_EXT,  90).
-define(      NEW_FLOAT_EXT,  70).
-define(        NEW_FUN_EXT, 112).
-define(        NEW_PID_EXT,  88).
-define(       NEW_PORT_EXT,  89).
-define(  NEW_REFERENCE_EXT, 114).
-define(            NIL_EXT, 106).
-define(            PID_EXT, 103).
-define(           PORT_EXT, 102).
-define(      REFERENCE_EXT, 101). % deprecated
-define(     SMALL_ATOM_EXT, 115). % deprecated
-define(SMALL_ATOM_UTF8_EXT, 119).
-define(      SMALL_BIG_EXT, 110).
-define(  SMALL_INTEGER_EXT,  97).
-define(    SMALL_TUPLE_EXT, 104).
-define(         STRING_EXT, 107).
-define(        V4_PORT_EXT, 120).

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
    decode_header(Data, Opts, #state{}).

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
    decode_terms(Rest, Opts, Buffer).

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
    {ok, Data};
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
% wildcard pattern
%---------------------------------------------------------------------
decode_terms(<<Code, _/binary>>, _Opts, _Buffer) ->
    {error, {unsupported, Code}}.

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

decode_small_integer_ext(<<Integer, Rest/binary>>, _Opts) ->
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

decode_integer_ext(<<Integer:32/signed-integer, Rest/binary>>, _Opts) ->
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
%% @end
%%--------------------------------------------------------------------
-spec decode_float_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, float(), binary()}.

decode_float_ext(<<Float:31/binary, Rest/binary>>, _Opts) ->
    NewFloat = binary_to_float(Float),
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
    {ok, NewAtom, Rest2}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
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
    {ok, NewAtom, Rest2}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
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
    {ok, NewAtom, Rest2}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
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
    {ok, NewAtom, Rest2}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
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

decode_small_tuple_ext2(0, Rest, _Opts, Buffer) ->
    Tuple = erlang:list_to_tuple(lists:reverse(Buffer)),
    {ok, Tuple, Rest};
decode_small_tuple_ext2(Arity, Rest, Opts, Buffer) ->
    {ok, Term, Rest2} = decode_terms(Rest, Opts, #state{}),
    decode_small_tuple_ext2(Arity-1, Rest2, Opts, [Term|Buffer]).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
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

decode_large_tuple_ext2(0, Rest, _Opts, Buffer) ->
    Tuple = erlang:list_to_tuple(lists:reverse(Buffer)),
    {ok, Tuple, Rest};
decode_large_tuple_ext2(Arity, Rest, Opts, Buffer) ->
    {ok, Term, Rest2} = decode_terms(Rest, Opts, #state{}),
    decode_large_tuple_ext2(Arity-1, Rest2, Opts, [Term|Buffer]).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_string_ext(<<Length:16/unsigned-integer, Characters/binary>>, Opts) ->
    decode_string_ext2(Length, Characters, Opts, "").

decode_string_ext2(0, Rest, _Opts, Buffer) ->
    {ok, lists:reverse(Buffer), Rest};
decode_string_ext2(Length, <<Character:8/unsigned-integer, Rest/binary>>, Opts, Buffer) ->
    decode_string_ext2(Length-1, Rest, Opts, [Character|Buffer]).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_list_ext(<<Length:32/unsigned-integer, Elements/binary>>, Opts) ->
    {ok, List, <<106, Rest/binary>>} = decode_list_ext2(Length, Elements, Opts, []),
    {ok, List, Rest}.

decode_list_ext2(0, Rest, _Opts, Buffer) ->
    {ok, lists:reverse(Buffer), Rest};
decode_list_ext2(Length, Elements, Opts, Buffer) ->
    {ok, Term, Rest} = decode_terms(Elements, Opts, #state{}),
    decode_list_ext2(Length-1, Rest, Opts, [Term|Buffer]).

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
encode(_Data, _Opts) ->
    ok.
