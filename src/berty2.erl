%%%===================================================================
%%% @copyright Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%===================================================================
-module(berty2).
-compile(export_all).
-include("berty.hrl").
-include_lib("kernel/include/logger.hrl").
-record(state, { id = make_ref()
               , module = ?MODULE 
               , started_at = undefined
               , ended_at = undefined
               }).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
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
%%
%%--------------------------------------------------------------------
decode(Data) -> decode(Data, #{}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
decode(<<131, Data/binary>>, Opts) -> 
    State = #state{ started_at = erlang:monotonic_time() },
    case decode(Data, Opts, State) of
        {ok, Term, <<>>} -> {ok, Term};
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
decode(<<First:8/unsigned-integer, _/binary>> = Data, Opts, #state{ module = Module } = State) ->
    Parser = code(First),
    Module:decode(Parser, Data, Opts, State).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------

%---------------------------------------------------------------------
% small_integer_ext => enabled | disabled | {callback, Callback}
% small_integer_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(small_integer_ext, <<?SMALL_INTEGER_EXT, Integer/signed-integer, Rest/binary>>, #{ small_integer_ext := cursed } = _Opts, _State) ->
    Result = binary_to_term(<<131, ?SMALL_INTEGER_EXT, Integer/integer>>, [safe]),
    {ok, Result, Rest};
decode(small_integer_ext, <<?SMALL_INTEGER_EXT, Integer/signed-integer, Rest/binary>>, _Opts, _State) ->
    {ok, Integer, Rest};

%---------------------------------------------------------------------
% integer_ext => enabled | disabled | {callback, Callback}
% integer_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(integer_ext, <<?INTEGER_EXT, Integer:32/signed-integer, Rest/binary>>, _Opts, _State) ->
    {ok, Integer, Rest};

%---------------------------------------------------------------------
% float_ext => enabled | disabled | cursed | {callback, Callback}
% float_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(float_ext, <<?FLOAT_EXT, Float:31/binary, Rest/binary>>, #{ minor_version := 0
                                                                 , float_ext := cursed } = _Opts, _State) ->
    Result = binary_to_term(<<131, ?FLOAT_EXT, Float:31/binary>>, [safe]),
    {ok, Result, Rest};
decode(float_ext, <<?FLOAT_EXT, Float:31/binary, Rest/binary>>, #{ minor_version := 0 } = _Opts, _State) ->
    NewFloat = binary_to_float(Float),
    {ok, NewFloat, Rest};

%---------------------------------------------------------------------
% new_float_ext => enabled | disabled | cursed | {callback, Callback}
% new_float_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(new_float_ext, <<?NEW_FLOAT_EXT, Float:8/binary, Rest/binary>>, #{ new_float_ext := cursed } = Opts, _State) ->
    Result = binary_to_term(<<131, ?NEW_FLOAT_EXT, Float/binary>>),
    {ok, Result, Rest};

%---------------------------------------------------------------------
% atom_utf8_ext => disabled | enabled | cursed | {callback, Callback}
% atom_utf8_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(atom_cache_ref, <<?ATOM_CACHE_REF, Rest/binary>>, _, _) -> 
    {error, {unsupported, atom_cache_ref}};

%---------------------------------------------------------------------
% atom_ext => enabled | disabled | cursed | {callback, Callback}
% atom_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(atom_ext, <<?ATOM_EXT, Length:16/unsigned-integer, Rest/binary>>, #{ atom_ext := disabled } = _Opts, _State) ->
    <<_Atom:Length/binary, Rest2/binary>> = Rest,
    {next, Rest2};
decode(atom_ext, <<?ATOM_EXT, Length:16/unsigned-integer, Rest/binary>>, #{ atom_ext := cursed } = _Opts, _State) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = binary_to_term(<<131, ?ATOM_EXT, Length:16/unsigned-integer, Atom:Length/binary>>, [safe]),
    {ok, NewAtom, Rest2};
decode(atom_ext, <<?ATOM_EXT, Length:16/unsigned-integer, Rest/binary>>, Opts, _State) -> 
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = decode_atoms(Atom, Opts),
    {ok, NewAtom, Rest2};

%---------------------------------------------------------------------
% small_atom_ext => enabled | disabled | cursed | {callback, Callback}
% small_atom_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(small_atom_ext, <<?SMALL_ATOM_EXT, Length:8/unsigned-integer, Rest/binary>>, #{ small_atom_ext := disabled } = Opts, State) -> 
    <<_Atom:Length, Rest2/binary>> = Rest,
    {next, Rest2};
decode(small_atom_ext, <<?SMALL_ATOM_EXT, Length:8/unsigned-integer, Rest/binary>>, #{ small_atom_ext := cursed } = Opts, State) -> 
    <<Atom:Length, Rest2/binary>> = Rest,
    NewAtom = binary_to_term(<<131, ?SMALL_ATOM_EXT, Length:8/unsigned-integer, Atom:Length/binary>>),
    {ok, NewAtom, Rest2};
decode(small_atom_ext, <<?SMALL_ATOM_EXT, Length:8/unsigned-integer, Rest/binary>>, Opts, State) -> 
    <<Atom:Length, Rest2/binary>> = Rest,
    NewAtom = decode_atoms(Atom, Opts),
    {ok, NewAtom, Rest2};

%---------------------------------------------------------------------
% small_atom_utf8_ext => enabled | disabled | cursed | {callback, Callback}
% small_atom_utf8_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(small_atom_utf8_ext, <<?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Rest/binary>>, #{ small_atom_utf8_ext := disabled } = Opts, _State) -> 
    <<_Atom:Length/binary, Rest2/binary>> = Rest,
    {next, Rest2};
decode(small_atom_utf8_ext, <<?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Rest/binary>>, #{ small_atom_utf8_ext := cursed } = Opts, _State) -> 
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = binary_to_term(<<131, ?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Atom:Length/binary>>, [safe]),
    {ok, NewAtom, Rest2};    
decode(small_atom_utf8_ext, <<?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Rest/binary>>, Opts, _State) -> 
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = decode_atoms(Atom, Opts),
    {ok, NewAtom, Rest2};

%---------------------------------------------------------------------
% atoms =>   {whitelist [binary(), ...]}
%          | create
%          | {create, Limit}
%          | {create, Limit, warning}
%          | as_string
%          | as_binary
%          | existing
% atom_utf8_ext => enabled | disabled | cursed | {callback, Callback}
% atom_utf8_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(atom_utf8_ext, <<?ATOM_UTF8_EXT, Length:16/unsigned-integer, Rest/binary>>, #{ atom_utf8_ext := disabled } = Opts, _State) -> 
    <<_Atom:Length/binary, Rest2/binary>> = Rest,
    {next, Rest2};
decode(atom_utf8_ext, <<?ATOM_UTF8_EXT, Length:16/unsigned-integer, Rest/binary>>, #{ atom_utf8_ext := cursed } = Opts, _State) -> 
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = binary_to_term(<<131, ?ATOM_UTF8_EXT, Length:16/unsigned-integer, Atom:Length/binary>>, [safe]),
    {ok, NewAtom, Rest2};
decode(atom_utf8_ext, <<?ATOM_UTF8_EXT, Length:16/unsigned-integer, Rest/binary>>, Opts, _State) -> 
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = decode_atoms(Atom, Opts),
    {ok, NewAtom, Rest2};

%---------------------------------------------------------------------
% small_tuple_ext => enabled | disabled | {callback, Callback}
% small_tuple_ext_arity => {Min, Max}
% small_tuple_ext_options => #{} % overwrite main options
%---------------------------------------------------------------------
decode(small_tuple_ext, <<?SMALL_TUPLE_EXT, Rest/binary>>, Opts, State) -> {error, unsupported};

%---------------------------------------------------------------------
% large_tuple_ext => enabled | disabled | {callback, Callback}
% large_tuple_ext_arity => {Min, Max}
% large_tuple_ext_options => #{} % overwrite main options
%---------------------------------------------------------------------
decode(large_tuple_ext, <<?LARGE_TUPLE_EXT, Rest/binary>>, Opts, State) -> {error, unsupported};

%---------------------------------------------------------------------
% string_ext => enabled | disabled | {callback, Callback}
% string_ext_length => {Min, Max}
% string_ext_options => #{} % overwrite main options
%---------------------------------------------------------------------
decode(string_ext, <<?STRING_EXT, Rest/binary>>, Opts, State) -> {error, unsupported};

%---------------------------------------------------------------------
% nil_ext => enabled | disabled | {callback, Callback}
%---------------------------------------------------------------------
decode(nil_ext, <<?NIL_EXT, Rest/binary>>, Opts, State) -> {error, unsupported};

%---------------------------------------------------------------------
% list_ext => enabled | disabled | {callback, Callback}
% list_ext_length => {Min, Max}
% list_ext_options = #{} % overwrite main options
%---------------------------------------------------------------------
decode(list_ext, <<?LIST_EXT, Rest/binary>>, Opts, State) -> {error, unsupported};

%---------------------------------------------------------------------
% binary_ext => enabled | disabled | {callback, Callback}
% binary_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(binary_ext, <<?BINARY_EXT, Rest/binary>>, Opts, State) -> {error, unsupported};

%---------------------------------------------------------------------
% bit_binary_ext => enabled | disabled | {callback, Callback}
% bit_binary_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(bit_binary_ext, <<?BIT_BINARY_EXT, Rest/binary>>, Opts, State) -> {error, unsupported};

%---------------------------------------------------------------------
% map_ext => enabled | disabled | {callback, Callback}
% map_ext_length => {Min, Max}
% map_ext_key_options => #{} % overwrite main options
% map_ext_value_options => #{} % overwrite main options
%---------------------------------------------------------------------
decode(map_ext, <<?MAP_EXT, Rest/binary>>, Opts, State) -> {error, unsupported};

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(Parser,_,_,_) ->
    {error, {unsupported, Parser}}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
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
