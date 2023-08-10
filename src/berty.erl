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
-include_lib("kernel/include/logger.hrl").
-record(state, { header = ""
               , compressed = false
               , data = []
               }).

%%--------------------------------------------------------------------
%% @doc 
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
% nil
decode_terms(<<106, Rest/binary>>, Opts, Buffer) ->
    decode_terms(Rest, Opts, Buffer#state{ data = nil });
% small_integer_ext
decode_terms(<<97, Rest/binary>>, Opts, Buffer) ->
    {ok, Integer, Rest2} = decode_small_integer_ext(Rest, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = Integer });
% integer_ext
decode_terms(<<98, Rest/binary>>, Opts, Buffer) ->
    {ok, Integer, Rest2} = decode_integer_ext(Rest, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = Integer});
% float_ext
decode_terms(<<99, Rest/binary>>, Opts, Buffer) ->
    {ok, Float, Rest2} = decode_float_ext(Rest, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = Float });
% atom_utf8_ext
decode_terms(<<119, Rest/binary>>, Opts, Buffer) ->
    {ok, Atom, Rest2} = decode_atom_utf8_ext(Rest, Opts),
    NewAtom = decode_atoms(Atom, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = NewAtom });
% atom_ext
decode_terms(<<100, Rest/binary>>, Opts, Buffer) ->
    {ok, Atom, Rest2} = decode_atom_ext(Rest, Opts),
    NewAtom = decode_atoms(Atom, Opts),
    decode_terms(Rest2, Opts, Buffer#state{ data = NewAtom }).

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

decode_atoms(Binary, #{ atoms := create }) ->
    erlang:binary_to_atom(Binary);
decode_atoms(Binary, #{ atoms := {create, Limit} }) 
  when is_float(Limit) andalso Limit >= 0 andalso Limit =< 1 ->
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
%% @end
%%--------------------------------------------------------------------
-spec decode_small_integer_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, integer(), binary()}.

decode_small_integer_ext(<<Integer, Rest/binary>>, _Opts) ->
    {ok, Integer, Rest}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode_integer_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, integer(), binary()}.

decode_integer_ext(<<Integer:32, Rest/binary>>, _Opts) ->
    {ok, Integer, Rest}.

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
    {ok, Float, Rest}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode_new_float_ext(Binary, Opts) -> Return when
      Binary :: binary(),
      Opts :: map(),
      Return :: {ok, float(), binary()}.

decode_new_float_ext(<<70, Float:8/binary, Rest/binary>>, _Opts) ->
    {ok, Float, Rest}.

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

decode_atom_utf8_ext(<<Length:16/unsigned-integer, Rest/binary>>, _Opts) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    {ok, Atom, Rest}.

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

decode_small_atom_utf8_ext(<<119, Length/unsigned-integer, Rest/binary>>, Opts) ->    
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    {ok, Atom, Rest}.

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
    {ok, Atom, Rest2}.

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
    {ok, Atom, Rest}.

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

decode_small_tuple_ext2(0, Rest ,Opts, Buffer) ->
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
encode(Data, Opts) ->
    ok.
