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
      Return :: {ok, term()}.

decode_terms(<<>>, _Opts, #state{ data = Data} = _Buffer) ->
    {ok, Data};
decode_terms(_, _Opts, #state{ data = Data } = _Buffer )
  when is_number(Data) orelse is_atom(Data) ->
    {ok, Data};

%---------------------------------------------------------------------
% nil support
%---------------------------------------------------------------------
decode_terms(<<106, Rest/binary>>, Opts, Buffer) ->
    decode_terms(Rest, Opts, Buffer#state{ data = nil });

%---------------------------------------------------------------------
% integers support
%---------------------------------------------------------------------
% small_integer_ext
decode_terms(<<97, Rest/binary>>, Opts, Buffer) ->
    {ok, Integer, Rest2} = decode_small_integer_ext(Rest, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = Integer });
% integer_ext
decode_terms(<<98, Rest/binary>>, Opts, Buffer) ->
    {ok, Integer, Rest2} = decode_integer_ext(Rest, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = Integer});

%---------------------------------------------------------------------
% floats support
%---------------------------------------------------------------------
% float_ext
decode_terms(<<99, Rest/binary>>, #{ minor_version := 0 } = Opts, Buffer) ->
    {ok, Float, Rest2} = decode_float_ext(Rest, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = Float });

%---------------------------------------------------------------------
% atoms support
%---------------------------------------------------------------------
% atom_ext
decode_terms(<<100, Rest/binary>>, Opts, Buffer) ->
    ?LOG_WARNING("ATOM_EXT is deprecated", []),
    {ok, Atom, Rest2} = decode_atom_ext(Rest, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = Atom });
% small_atom_ext
decode_terms(<<115, Rest/binary>>, Opts, Buffer) ->
    ?LOG_WARNING("SMALL_ATOM_EXT is deprecated", []),
    {ok, Atom, Rest2} = decode_small_atom_ext(Rest, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = Atom });
% atom_utf8_ext
decode_terms(<<118, Rest/binary>>, Opts, Buffer) ->
    {ok, Atom, Rest2} = decode_atom_utf8_ext(Rest, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = Atom });
% small_atom_utf8_ext
decode_terms(<<119, Rest/binary>>, Opts, Buffer) ->
    {ok, Atom, Rest2} = decode_small_atom_utf8_ext(Rest, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = Atom });

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

decode_small_tuple_ext(<<104, Arity, Rest/binary>>, Opts) ->
    decode_small_tuple_ext2(Arity, Rest, Opts, []).

decode_small_tuple_ext2(0, Rest, _Opts, Buffer) ->
    {ok, Buffer, Rest};
decode_small_tuple_ext2(Arity, Rest, Opts, Buffer) ->
    decode_small_tuple_ext2(Arity-1, Rest, Opts, Buffer).


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
