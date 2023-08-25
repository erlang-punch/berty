%%%===================================================================
%%% @copyright Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%%
%%% This module is the one used to decode (and encode in a near
%%% future) Erlang External Term Format. By default, only a small
%%% subset of ETF. By default, it works like `binary_to_term/1` and
%%% `binary_to_term/2` functions.
%%%
%%% ```
%%% % decoding simple term with default configuration
%%% {ok, {1,2}} = berty:decode(term_to_binary({1,2}).
%%% '''
%%%
%%% A term can be decoded in many different ways.
%%%
%%% <ul>
%%%   <li>`enabled': use a default and safe method to decode a term;</li>
%%%   <li>`disabled': stop the parser;</li>
%%%   <li>`cursed': use `binary_to_term/2' function with `[safe]' option;</li>
%%%   <li>`{callback, Callback}': use a lambda function with arity 4
%%%       or a module/function tuple `{Module, Function}';</li>
%%%   <li>`drop': just drop the term and do the next one;</li>
%%%   <li>`raw': mainly for internal use, but can be used for specific usages.</li>
%%% </ul>
%%%
%%% ```
%%% % define a callback function
%%% Fun = fun(RawTerm, Rest, Opts, State) ->
%%%   {ok, unsupported, Rest}
%%% end.
%%%
%%% % use the callback function to decode small_integers
%%% Opts = #{ small_integer_ext => {callback, Fun}.
%%% {ok, {unsupported, unsupported}}
%%%   = berty:decode(term_to_binary({1,2}, Opts).
%%% '''
%%%
%%% == Atoms Protection ==
%%%
%%% Different kind of atoms can be supported and protected. If a
%%% global protection is required, `atoms` option is probably what you
%%% are looking for.
%%%
%%% ```
%%% '''
%%%
%%% == Memory Protection ==
%%%
%%% @end
%%%===================================================================
-module(berty_etf).
-export([default_options/0]).
-export([decode/1, decode/2]).
-include("berty.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%---------------------------------------------------------------------
%
%--------------------------------------------------------------------
-record(state, { id = make_ref()
               , module = ?MODULE
               , started_at = undefined
               , ended_at = undefined
               , depth = 0
               , metric_process = berty_metrics
               }).

%%--------------------------------------------------------------------
%% @hidden
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

default_options() ->
    #{ small_integer_ext => enabled
     , integer_ext => enabled
     , float_ext => cursed
     , new_float_ext => cursed
     , atom_utf8_ext => enabled
     , atom_ext => enabled
     , small_atom_ext => enabled
     , small_atom_utf8_ext => enabled
     , small_tuple_ext => enabled
     , small_big_ext => cursed
     , large_big_ext => cursed
     , large_tuple_ext => enabled
     , string_ext => enabled
     , nil_ext => enabled
     , list_ext => enabled
     , binary_ext => enabled
     , bit_binary_ext => enabled
     , map_ext => enabled
     }.

-type common_options() :: enabled | disabled | cursed | drop | raw.
-type callback_option() :: {callback, function() | {atom(), atom()}}.
-type limit_option() :: {number(), number()}.
-type atom_ext() :: common_options() | callback_option().
-type atom_ext_size() :: limit_option().
-type atom_utf8_ext() ::  common_options() | callback_option().
-type atom_utf8_ext_size() :: limit_option().
-type binary_ext() :: common_options() | callback_option().
-type binary_ext_size() :: limit_option().
-type bit_binary_ext() :: enabled | disabled | callback_option().
-type bit_binary_ext_size() :: limit_option().
-type float_ext_limit() :: limit_option().
-type float_ext() ::  enabled | disabled | cursed | callback_option().
-type integer_ext_limit() :: limit_option().
-type integer_ext() ::  enabled | disabled | callback_option().
-type large_tuple_ext_arity() :: limit_option().
-type large_tuple_ext() :: enabled | disabled | callback_option().
-type list_ext_length() :: limit_option().
-type list_ext() :: enabled | disabled | callback_option().
-type map_ext_length() :: limit_option().
-type map_ext() ::  enabled | disabled | callback_option().
-type new_float_ext_limit() :: limit_option().
-type new_float_ext() ::  enabled | disabled | cursed | callback_option().
-type new_pid_ext() :: enabled | disabled | cursed | callback_option().
-type new_port_ext() :: enabled | disabled | cursed | callback_option().
-type new_reference_ext() :: enabled | disabled | cursed | callback_option().
-type newer_reference_ext() :: enabled | disabled | cursed | callback_option().
-type nil_ext() :: enabled | disabled | callback_option().
-type pid_ext() :: enabled | disabled | cursed | callback_option().
-type port_ext() :: enabled | disabled | cursed | callback_option().
-type small_atom_ext() ::  enabled | disabled | cursed | callback_option().
-type small_atom_ext_size() :: limit_option().
-type small_atom_utf8_ext() :: enabled | disabled | cursed | callback_option().
-type small_atom_utf8_ext_size() :: limit_option().
-type small_integer_ext_limit() :: limit_option().
-type small_integer_ext() :: enabled | disabled | drop | cursed | callback_option().
-type small_tuple_ext_arity() :: limit_option().
-type small_tuple_ext() :: enabled | disabled | callback_option().
-type string_ext_length() :: limit_option().
-type string_ext() :: enabled | disabled | callback_option().
-type v4_port_ext() :: enabled | disabled | cursed | callback_option().

-type atoms() :: create
               | {create, number()}
               | {create, number(), warning}
               | as_string
               | as_binary
               | existing.

-type options() :: #{ atom_ext => atom_ext()
                    , {atom_ext, size} => atom_ext_size()
                    , atom_utf8_ext  => atom_utf8_ext()
                    , {atom_utf8_ext, size} => atom_utf8_ext_size()
                    , binary_ext => binary_ext()
                    , {binary_ext, size} => binary_ext_size()
                    , bit_binary_ext => bit_binary_ext()
                    , bit_binary_ext_size => bit_binary_ext_size()
                    , float_ext => float_ext()
                    , float_ext_limit => float_ext_limit()
                    , integer_ext => integer_ext()
                    , {integer_ext, limit} => integer_ext_limit()
                    , {large_tuple_ext, arity} => large_tuple_ext_arity()
                    , large_tuple_ext => large_tuple_ext()
                    , {list_ext, length} => list_ext_length()
                    , list_ext => list_ext()
                    , {map_ext, length} => map_ext_length()
                    , map_ext => map_ext()
                    , new_float_ext => new_float_ext()
                    , {new_float_ext, limit} => new_float_ext_limit()
                    , new_pid_ext => new_pid_ext()
                    , new_port_ext => new_port_ext()
                    , new_reference_ext => new_reference_ext()
                    , newer_reference_ext => newer_reference_ext()
                    , nil_ext => nil_ext()
                    , pid_ext => pid_ext()
                    , port_ext => port_ext()
                    , small_atom_ext => small_atom_ext()
                    , {small_atom_ext, size} => small_atom_ext_size()
                    , small_atom_utf8_ext => small_atom_utf8_ext()
                    , small_atom_utf8_ext_size => small_atom_utf8_ext_size()
                    , {small_integer_ext, limit} => small_integer_ext_limit()
                    , small_integer_ext => small_integer_ext()
                    , {small_tuple_ext, arity} => small_tuple_ext_arity()
                    , small_tuple_ext => small_tuple_ext()
                    , {string_ext, length} => string_ext_length()
                    , string_ext => string_ext()
                    , v4_port_ext => v4_port_ext()
                    , atoms => atoms()
                    }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec decode(Data) -> Return when
      Data :: binary(),
      Return :: {ok, term()}
              | {ok, term(), binary()}
              | {error, Reason, State},
      State :: #state{},
      Reason :: proplists:proplist().

decode(Data) ->
    decode(Data, default_options()).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec decode(Data, Opts) -> Return when
      Data :: binary(),
      Return :: {ok, term()}
              | {ok, term(), binary()}
              | {error, Reason, State},
      State :: #state{},
      Opts :: options(),
      Reason :: proplists:proplist().

decode(<<131, Data/binary>> = _Payload, Opts) ->
    State = #state{ started_at = erlang:monotonic_time() },
    DefaultOpts = default_options(),
    NewOpts = maps:merge(DefaultOpts, Opts),
    case decode(Data, NewOpts, State) of
        {ok, Term, <<>>} -> {ok, Term};
        {next, Rest} -> decode(Rest, NewOpts, State);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
decode(<<First:8/unsigned-integer, _/binary>> = Data, Opts, #state{ module = Module } = State) ->
    Parser = code(First),
    case Module =:= ?MODULE of
        true ->
            decode(Parser, Data, Opts, State);
        false ->
            Module:decode(Parser, Data, Opts, State)
    end.

%---------------------------------------------------------------------
% small_integer_ext => enabled | disabled | {callback, Callback}
% small_integer_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(small_integer_ext, <<?SMALL_INTEGER_EXT, Integer/unsigned-integer, Rest/binary>>
      , #{ small_integer_ext := cursed } = Opts, State) ->
    {Min, Max} = maps:get({small_integer_ext, limit}, Opts, {0,255}),
    if Integer >= Min andalso Integer =< Max ->
            Result = ?BINARY_TO_TERM(<<131, ?SMALL_INTEGER_EXT, Integer/integer>>),
            {ok, Result, Rest};
       true ->
            Reason = [{reason, "small_integer_ext limits"}],
            decode_error(Reason, Opts, State)
    end;
decode(small_integer_ext, <<?SMALL_INTEGER_EXT, _Integer/unsigned-integer, _Rest/binary>>
      , #{ small_integer_ext := disabled } = Opts, State) ->
    Reason = [{reason, "small_integer_ext disabled"}],
    decode_error(Reason, Opts, State);
decode(small_integer_ext, <<?SMALL_INTEGER_EXT, Integer/unsigned-integer, Rest/binary>>
      , #{ small_integer_ext := Context } = Opts, State) ->
    {Min, Max} = maps:get({small_integer_ext, limit}, Opts, {0,255}),
    if Integer >= Min andalso Integer =< Max ->
            case Context of
                drop ->
                    {next, Rest};
                raw ->
                    {raw, <<Integer/unsigned-integer>>, Rest};
                {callback, Callback}  ->
                    RawTerm = <<?SMALL_INTEGER_EXT, Integer/unsigned-integer>>,
                    decode_callback(Callback, RawTerm, Rest, Opts, State);
                _Elsewise ->
                    {ok, Integer, Rest}
            end;
       true ->
            {error, [{reason, "small_integer_ext limits"}], State}
    end;

%---------------------------------------------------------------------
% integer_ext => enabled | disabled | {callback, Callback}
% integer_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(integer_ext, <<?INTEGER_EXT, Integer:32/signed-integer, Rest/binary>>
      , #{ integer_ext := cursed }, _State) ->
    Result = ?BINARY_TO_TERM(<<131, ?INTEGER_EXT, Integer:32/integer>>),
    {ok, Result, Rest};
decode(integer_ext, <<?INTEGER_EXT, _Integer:32/signed-integer, _Rest/binary>>
      , #{ integer_ext := disabled }, State) ->
    {error, [{reason, "integer_ext disabled"}], State};
decode(integer_ext, <<?INTEGER_EXT, Integer:32/signed-integer, Rest/binary>>
      , #{ integer_ext := Context } = Opts, State) ->
    {Min, Max} = maps:get({small_integer_ext, limit}, Opts, {-2147483648,2147483647}),
    if Integer >= Min andalso Integer =< Max ->
            case Context of
                drop ->
                    {next, Rest};
                raw ->
                    {raw, <<Integer:32/signed-integer>>, Rest};
                {callback, Callback} ->
                    RawTerm = <<?INTEGER_EXT, Integer:32/signed-integer>>,
                    decode_callback(Callback, RawTerm, Rest, Opts, State);
                _Elsewise ->
                    {ok, Integer, Rest}
            end;
       true ->
            Reason = [{reason, "small_integer_ext limits"}],
            decode_error(Reason, Opts, State)
    end;

%---------------------------------------------------------------------
% float_ext => enabled | disabled | cursed | {callback, Callback}
% float_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(float_ext, <<?FLOAT_EXT, Float:31/binary, Rest/binary>>
      , #{ minor_version := 0, float_ext := cursed }, _State) ->
    Result = ?BINARY_TO_TERM(<<131, ?FLOAT_EXT, Float:31/binary>>),
    {ok, Result, Rest};
decode(float_ext, <<?FLOAT_EXT, Float:31/binary, Rest/binary>>
      , #{ minor_version := 0, float_ext := Context } = Opts, State) ->
    case Context of
        drop ->
            {next, Rest};
        raw ->
            {raw, Float, Rest};
        {callback, Callback} ->
            RawTerm = <<?FLOAT_EXT, Float:31/binary>>,
            decode_callback(Callback, RawTerm, Rest, Opts, State);
        enabled ->
            NewFloat = binary_to_float(Float),
            {ok, NewFloat, Rest};
        _ ->
            Reason = [{reason, "integer_ext unsupported"}],
            decode_error(Reason, Opts, State)
    end;

%---------------------------------------------------------------------
% new_float_ext => enabled | disabled | cursed | {callback, Callback}
% new_float_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(new_float_ext, <<?NEW_FLOAT_EXT, Float:8/binary, Rest/binary>>
      , #{ new_float_ext := cursed }, _State) ->
    Result = ?BINARY_TO_TERM(<<131, ?NEW_FLOAT_EXT, Float/binary>>),
    {ok, Result, Rest};
decode(new_float_ext, <<?NEW_FLOAT_EXT, Float:8/binary, Rest/binary>>
      , #{ new_float_ext := Context } = Opts, State) ->
    case Context of
        drop ->
            {next, Rest};
        raw ->
            {raw, Float, Rest};
        {callback, Callback} ->
            RawTerm = <<?NEW_FLOAT_EXT, Float:8/binary>>,
            decode_callback(Callback, RawTerm, Rest, Opts, State);
        _ ->
            {error, [{reason, unsupported}]}
    end;

%---------------------------------------------------------------------
% atom_ext => enabled | disabled | cursed | {callback, Callback}
% atom_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(atom_ext, <<?ATOM_EXT, Length:16/unsigned-integer, Rest/binary>>
      , #{ atom_ext := disabled }, _State) ->
    <<_Atom:Length/binary, Rest2/binary>> = Rest,
    {next, Rest2};
decode(atom_ext, <<?ATOM_EXT, Length:16/unsigned-integer, Rest/binary>>
      , #{ atom_ext := cursed }, _State) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = ?BINARY_TO_TERM(<<131, ?ATOM_EXT, Length:16/unsigned-integer, Atom:Length/binary>>),
    {ok, NewAtom, Rest2};
decode(atom_ext, <<?ATOM_EXT, Length:16/unsigned-integer, Rest/binary>>
      , #{ atom_ext := Context } = Opts, State) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    case Context of
        raw ->
            {raw, <<Length:16/unsigned-integer, Atom:Length/binary>>, Rest2};
        drop ->
            {next, Rest2};
        {callback, Callback} ->
            RawTerm = <<?ATOM_EXT, Length:16/unsigned-integer, Atom:Length/binary>>,
            decode_callback(Callback, RawTerm, Rest, Opts, State);
        _Elsewise ->
            NewAtom = decode_atoms(Atom, Opts),
            {ok, NewAtom, Rest2}
    end;

%---------------------------------------------------------------------
% small_atom_ext => enabled | disabled | cursed | {callback, Callback}
% small_atom_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(small_atom_ext, <<?SMALL_ATOM_EXT, Length:8/unsigned-integer, Rest/binary>>
      , #{ small_atom_ext := disabled }, _State) ->
    <<_Atom:Length, Rest2/binary>> = Rest,
    {next, Rest2};
decode(small_atom_ext, <<?SMALL_ATOM_EXT, Length:8/unsigned-integer, Rest/binary>>
      , #{ small_atom_ext := cursed }, _State) ->
    <<Atom:Length, Rest2/binary>> = Rest,
    NewAtom = ?BINARY_TO_TERM(<<131, ?SMALL_ATOM_EXT, Length:8/unsigned-integer, Atom:Length/binary>>),
    {ok, NewAtom, Rest2};
decode(small_atom_ext, <<?SMALL_ATOM_EXT, Length:8/unsigned-integer, Rest/binary>>
      , #{ small_atom_ext := Context } = Opts, State) ->
    <<Atom:Length, Rest2/binary>> = Rest,
    case Context of
        drop ->
            {next, Rest2};
        raw ->
            {raw, <<Length:8/unsigned-integer, Atom:Length/binary>>, Rest2};
        {callback, Callback} ->
            RawTerm = <<?SMALL_ATOM_EXT, Length:8/unsigned-integer, Atom:Length/binary>>,
            decode_callback(Callback, RawTerm, Rest2, Opts, State);
        enabled ->
            NewAtom = decode_atoms(Atom, Opts),
            {ok, NewAtom, Rest2}
    end;

%---------------------------------------------------------------------
% small_atom_utf8_ext => enabled | disabled | cursed | {callback, Callback}
% small_atom_utf8_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(small_atom_utf8_ext, <<?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Rest/binary>>
      , #{ small_atom_utf8_ext := disabled }, _State) ->
    <<_Atom:Length/binary, Rest2/binary>> = Rest,
    {next, Rest2};
decode(small_atom_utf8_ext, <<?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Rest/binary>>
      , #{ small_atom_utf8_ext := cursed }, _State) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = ?BINARY_TO_TERM(<<131, ?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Atom:Length/binary>>),
    {ok, NewAtom, Rest2};
decode(small_atom_utf8_ext, <<?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Rest/binary>>
      , #{ small_atom_utf8_ext := Context } = Opts, State) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    case Context of
        drop ->
            {next, Rest2};
        raw ->
            {raw, <<Length:8/unsigned-integer, Atom:Length/binary>>, Rest2};
        {callback, Callback} ->
            RawTerm = <<?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Atom:Length/binary>>,
            decode_callback(Callback, RawTerm, Rest2, Opts, State);
        enabled ->
            NewAtom = decode_atoms(Atom, Opts),
            {ok, NewAtom, Rest2}
    end;

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
decode(atom_utf8_ext, <<?ATOM_UTF8_EXT, Length:16/unsigned-integer, Rest/binary>>
      , #{ atom_utf8_ext := disabled }, _State) ->
    <<_Atom:Length/binary, Rest2/binary>> = Rest,
    {next, Rest2};
decode(atom_utf8_ext, <<?ATOM_UTF8_EXT, Length:16/unsigned-integer, Rest/binary>>
      , #{ atom_utf8_ext := cursed }, _State) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = ?BINARY_TO_TERM(<<131, ?ATOM_UTF8_EXT, Length:16/unsigned-integer, Atom:Length/binary>>),
    {ok, NewAtom, Rest2};
decode(atom_utf8_ext, <<?ATOM_UTF8_EXT, Length:16/unsigned-integer, Rest/binary>>
      , #{ atom_utf8_ext := Context } = Opts, State) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    case Context of
        drop ->
            {next, Rest2};
        raw ->
            {raw, <<Length:16/unsigned-integer, Atom:Length/binary>>, Rest2};
        {callback, Callback} ->
            RawTerm = <<?ATOM_UTF8_EXT, Length:16/unsigned-integer, Atom:Length/binary>>,
            decode_callback(Callback, RawTerm, Rest2, Opts, State);
        enabled ->
            NewAtom = decode_atoms(Atom, Opts),
            {ok, NewAtom, Rest2}
    end;

%---------------------------------------------------------------------
% small_tuple_ext => enabled | disabled | {callback, Callback}
% small_tuple_ext_arity => {Min, Max}
% small_tuple_ext_options => #{} % overwrite main options
%---------------------------------------------------------------------
decode(small_tuple_ext, <<?SMALL_TUPLE_EXT, 0/unsigned-integer, Rest/binary>>, _, _State) ->
    {ok, {}, Rest};
decode(small_tuple_ext, <<?SMALL_TUPLE_EXT, Arity/unsigned-integer, Rest/binary>>, Opts, State) ->
    {Min, Max} = maps:get({small_tuple_ext, limit}, Opts, {0,255}),
    TupleOpts = maps:get({small_tuple_ext, options}, Opts, #{}),
    NewOpts = maps:merge(Opts, TupleOpts),
    if Arity >= Min andalso Arity =< Max ->
            decode_tuple_ext(Arity, Rest, NewOpts, State);
       true ->
            {error, [{reason, "small_tuple_ext limits"}], State}
    end;

%---------------------------------------------------------------------
% large_tuple_ext => enabled | disabled | {callback, Callback}
% large_tuple_ext_arity => {Min, Max}
% large_tuple_ext_options => #{} % overwrite main options
%---------------------------------------------------------------------
decode(large_tuple_ext, <<?LARGE_TUPLE_EXT, 0:32/unsigned-integer, Rest/binary>>, _, _State) ->
    {ok, {}, Rest};
decode(large_tuple_ext, <<?LARGE_TUPLE_EXT, Arity:32/unsigned-integer, Rest/binary>>, Opts, State) ->
    decode_tuple_ext(Arity, Rest, Opts, State);

%---------------------------------------------------------------------
% string_ext => enabled | disabled | {callback, Callback}
% string_ext_length => {Min, Max}
% string_ext_options => #{} % overwrite main options
%---------------------------------------------------------------------
decode(string_ext, <<?STRING_EXT, 0:16/unsigned-integer, Rest/binary>>, _Opts, _State) ->
    {ok, "", Rest};
decode(string_ext, <<?STRING_EXT, Length:16/unsigned-integer, Rest/binary>>, Opts, State) ->
    decode_string_ext(Length, Rest, Opts, State);

%---------------------------------------------------------------------
% nil_ext => enabled | disabled | {callback, Callback}
%---------------------------------------------------------------------
decode(nil_ext, <<?NIL_EXT, Rest/binary>>, _Opts, _State) ->
    {ok, [], Rest};

%---------------------------------------------------------------------
% list_ext => enabled | disabled | {callback, Callback}
% list_ext_length => {Min, Max}
% list_ext_options = #{} % overwrite main options
%---------------------------------------------------------------------
decode(list_ext, <<?LIST_EXT, 0:32/unsigned-integer, Rest/binary>>, _Opts, _State) ->
    {ok, [], Rest};
decode(list_ext, <<?LIST_EXT, Length:32/unsigned-integer, Rest/binary>>, Opts, State) ->
    decode_list_ext(Length, Rest, Opts, State);

%---------------------------------------------------------------------
% binary_ext => enabled | disabled | cursed | {callback, Callback}
% binary_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(binary_ext, <<?BINARY_EXT, Size:32/unsigned-integer, Rest/binary>>
      , #{ binary_ext := cursed } = _Opts, _State) ->
    <<Binary:Size/binary, Rest2/binary>> = Rest,
    CursedBinary = ?BINARY_TO_TERM(<<131, ?BINARY_EXT, Size:32/unsigned-integer, Binary:Size/binary>>),
    {ok, CursedBinary, Rest2};
decode(binary_ext, <<?BINARY_EXT, Size:32/unsigned-integer, Rest/binary>>
      , #{ binary_ext := Context } = Opts, State) ->
    <<Binary:Size/binary, Rest2/binary>> = Rest,
    case Context of
        drop ->
            {next, Rest2};
        raw ->
            RawTerm = <<Size:32/unsigned-integer, Binary:Size/binary>>,
            {raw, RawTerm, Rest2};
        {callback, Callback} ->
            RawTerm = <<?BINARY_EXT, Size:32/unsigned-integer, Binary:Size/binary>>,
            decode_callback(Callback, RawTerm, Rest2, Opts, State);
        enabled ->
            {ok, Binary, Rest2}
    end;

%---------------------------------------------------------------------
% bit_binary_ext => enabled | disabled | {callback, Callback}
% bit_binary_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(bit_binary_ext, <<?BIT_BINARY_EXT, Size:32/unsigned-integer, Bits:8, Rest/binary>>
      , #{ bit_binary_ext := cursed } = _Opts, _State) ->
    <<Binary:(Size-1)/binary, Byte:1/binary, Rest2/binary>> = Rest,
    Cursed = <<131, ?BIT_BINARY_EXT, Size:32/unsigned-integer, Bits:8
              , Binary:(Size-1)/binary, Byte:1/binary>>,
    CursedBits = ?BINARY_TO_TERM(Cursed),
    {ok, CursedBits, Rest2};
decode(bit_binary_ext, <<?BIT_BINARY_EXT, Size:32/unsigned-integer, Bits:8, Rest/binary>>
      , #{ bit_binary_ext := Context } = Opts, State) ->
    <<Binary:(Size-1)/binary, Byte:1/binary, Rest2/binary>> = Rest,
    case Context of
        drop ->
            {next, Rest2};
        raw ->
            RawTerm = <<?BIT_BINARY_EXT, Size:32/unsigned-integer
                       , Binary:(Size-1)/binary, Byte:1/binary>>,
            {raw, RawTerm, Rest2};
        {callback, Callback} ->
            RawTerm = <<?BIT_BINARY_EXT, Size:32/unsigned-integer
                       , Binary:(Size-1)/binary, Byte:1/binary>>,
            decode_callback(Callback, RawTerm, Rest2, Opts, State);
        enabled ->
            {ok, <<Binary/binary, Byte:Bits/bitstring>>, Rest2}
    end;

%---------------------------------------------------------------------
% map_ext => enabled | disabled | {callback, Callback}
% map_ext_length => {Min, Max}
% map_ext_key_options => #{} % overwrite main options
% map_ext_value_options => #{} % overwrite main options
%---------------------------------------------------------------------
decode(map_ext, <<?MAP_EXT, Arity:32/unsigned-integer, Rest/binary>>, Opts, State) ->
    decode_map_ext(Arity, Rest, Opts, State);

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(port_ext, <<?PORT_EXT, Rest/binary>>, #{ port_ext := cursed } = Opts, State) ->
    {ok, _Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
    <<Id:32/unsigned-integer, Creation:8/unsigned-integer, Rest3/binary>> = Rest2,
    Format = io_lib:format("#Port<~B.~B>", [Creation,Id]),
    StringPort = lists:flatten(Format),
    {ok, list_to_port(StringPort), Rest3};
decode(port_ext, <<?PORT_EXT, Rest/binary>>, #{ port_ext := Context } = Opts, State) ->
    {raw, Node, Rest2} = decode(atom_ext, Rest, Opts#{ atom_ext => raw }, State),
    <<Id:32/unsigned-integer, Creation:8/unsigned-integer, Rest3/binary>> = Rest2,
    case Context of
        drop ->
            {next, Rest3};
        raw ->
            RawTerm = <<Node/binary, Id:32/unsigned-integer, Creation:8/unsigned-integer>>,
            {raw, RawTerm, Rest3};
        {callback, Callback} ->
            RawTerm = <<?PORT_EXT, Node/binary, Id:32/unsigned-integer, Creation:8/unsigned-integer>>,
            decode_callback(Callback, RawTerm, Rest3, Opts, State);
        _ ->
            Reason = [{reason, "port_ext disabled"}],
            decode_error(Reason, Opts, State)
    end;

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(new_port_ext, <<?NEW_PORT_EXT, Rest/binary>>, #{ new_port_ext := cursed } = Opts, State) ->
    {ok, _Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
    <<Id:32/unsigned-integer, Creation:32/unsigned-integer, Rest3/binary>> = Rest2,
    Format = io_lib:format("#Port<~B.~B>", [Creation,Id]),
    StringPort = lists:flatten(Format),
    {ok, list_to_port(StringPort), Rest3};
decode(new_port_ext, <<?NEW_PORT_EXT, Rest/binary>>, #{ new_port_ext := Context } = Opts, State) ->
    {raw, Node, Rest2} = decode(atom_ext, Rest, Opts#{ atom_ext => raw }, State),
    <<Id:32/unsigned-integer, Creation:32/unsigned-integer, Rest3/binary>> = Rest2,
    case Context of
        drop ->
            {next, Rest3};
        raw ->
            RawTerm = <<Node/binary, Id:32/unsigned-integer, Creation:32/unsigned-integer>>,
            {raw, RawTerm, Rest3};
        {callback, Callback} ->
            RawTerm = <<?NEW_PORT_EXT, Node/binary, Id:32/unsigned-integer, Creation:32/unsigned-integer>>,
            decode_callback(Callback, RawTerm, Rest3, Opts, State);
        _ ->
            Reason = [{reason, "port_ext disabled"}],
            decode_error(Reason, Opts, State)
    end;

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(v4_port_ext, <<?V4_PORT_EXT, Rest/binary>>, #{ v4_port_ext := cursed } = Opts, State) ->
    {ok, _Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
    <<Id:64/unsigned-integer, Creation:32/unsigned-integer, Rest3/binary>> = Rest2,
    Format = io_lib:format("#Port<~B.~B>", [Creation,Id]),
    StringPort = lists:flatten(Format),
    {ok, list_to_port(StringPort), Rest3};
decode(v4_port_ext, <<?V4_PORT_EXT, Rest/binary>>, #{ v4_port_ext := Context } = Opts, State) ->
    {raw, Node, Rest2} = decode(atom_ext, Rest, Opts#{ atom_ext => raw }, State),
    <<Id:64/unsigned-integer, Creation:32/unsigned-integer, Rest3/binary>> = Rest2,
    case Context of
        drop ->
            {next, Rest3};
        raw ->
            RawTerm = <<Node/binary, Id:64/unsigned-integer, Creation:32/unsigned-integer>>,
            {raw, RawTerm, Rest3};
        {callback, Callback} ->
            RawTerm = <<?V4_PORT_EXT, Node/binary, Id:64/unsigned-integer, Creation:32/unsigned-integer>>,
            decode_callback(Callback, RawTerm, Rest3, Opts, State);
        _ ->
            Reason = [{reason, "v4_port_ext disabled"}],
            decode_error(Reason, Opts, State)
    end;

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(pid_ext, <<?PID_EXT, Rest/binary>>, #{ pid_ext := cursed } = Opts, State) ->
    {ok, _Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
    <<ID:32/unsigned-integer, Serial:32/unsigned-integer
     ,Creation:8/unsigned-integer, Rest3/binary>> = Rest2,
    Pid = c:pid(Creation, ID, Serial),
    {ok, Pid, Rest3};
decode(pid_ext, <<?PID_EXT, Rest/binary>>, #{ pid_ext := Context } = Opts, State) ->
    {raw, Node, Rest2} = decode(atom_ext, Rest, Opts#{ atom_ext => raw }, State),
    <<ID:32/unsigned-integer, Serial:32/unsigned-integer
     ,Creation:8/unsigned-integer, Rest3/binary>> = Rest2,
    case Context of
        drop -> {next, Rest3};
        raw ->
            RawTerm = <<Node/binary, ID:32/unsigned-integer
                       , Serial:32/unsigned-integer, Creation:8/unsigned-integer>>,
            {raw, RawTerm, Rest3};
        {callback, Callback} ->
            RawTerm = <<?PID_EXT, Node/binary, ID:32/unsigned-integer
                       , Serial:32/unsigned-integer, Creation:8/unsigned-integer>>,
            decode_callback(Callback, RawTerm, Rest3, Opts, State);
        _ ->
            Reason = [{reason, "pid_ext disabled"}],
            decode_error(Reason, Opts, State)
    end;

%---------------------------------------------------------------------
% @todo to be tested
% fun_ext has been removed since R23, this implementation only returns
% the content of the fun, only in cursed mode.
%---------------------------------------------------------------------
% decode(fun_ext, <<?FUN_EXT, _NumFree:32/unsigned-integer, Rest/binary>>, #{ fun_ext := cursed } = Opts, State) ->
%     {ok, Pid, RestPid} = decode(pid_ext, Rest, Opts#{ pid_ext => cursed }, #state{}),
%     {ok, Module, RestModule} = decode(RestPid, Opts#{ atoms => create }, #state{}),
%     {ok, Index, RestIndex} = decode(RestModule, Opts, #state{}),
%     {ok, Uniq, RestUniq} = decode(RestIndex, Opts, #state{}),
%     {ok, FreeVars, RestFreeVars} = decode(RestUniq, Opts#{ atoms => create }, #state{}),
%     {ok, {fun_ext, {Pid, Module, Index, Uniq, FreeVars}, RestFreeVars}};
decode(fun_ext, <<?FUN_EXT, _NumFree:32/unsigned-integer, _Rest/binary>>, #{ fun_ext := _Context } = Opts, State) ->
    Reason = [{reason, "fun_ext disabled"}],
    decode_error(Reason, Opts, State);

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(new_pid_ext, <<?NEW_PID_EXT, Rest/binary>>
      ,#{ new_pid_ext := cursed } = Opts, State) ->
    {ok, _Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
    <<ID:32/unsigned-integer, Serial:32/unsigned-integer
     ,Creation:32/unsigned-integer, Rest3/binary>> = Rest2,
    Pid = c:pid(Creation, ID, Serial),
    {ok, Pid, Rest3};
decode(new_pid_ext, <<?NEW_PID_EXT, Rest/binary>>
      ,#{ new_pid_ext := Context } = Opts, State) ->
    {raw, Node, Rest2} = decode(atom_ext, Rest, Opts#{ atom_ext => raw }, State),
    <<ID:32/unsigned-integer, Serial:32/unsigned-integer
     ,Creation:32/unsigned-integer, Rest3/binary>> = Rest2,
    case Context of
        drop ->
            {next, Rest3};
        raw ->
            RawTerm = <<Node/binary, ID:32/unsigned-integer, Serial:32/unsigned-integer
                       ,Creation:32/unsigned-integer>>,
            {raw, RawTerm, Rest3};
        {callback, Callback} ->
            RawTerm = <<?NEW_PID_EXT, Node/binary, ID:32/unsigned-integer
                       , Serial:32/unsigned-integer,Creation:32/unsigned-integer>>,
            decode_callback(Callback, RawTerm, Rest3, Opts, State);
        _ ->
            Reason = [{reason, "new_pid_ext disabled"}],
            decode_error(Reason, Opts, State)
    end;

%---------------------------------------------------------------------
% @todo to be tested
%---------------------------------------------------------------------
% -type reference_ext() :: enabled | disabled | cursed | callback_option().
% decode(reference_ext, <<?REFERENCE_EXT, Rest/binary>>
%       ,#{ reference_ext := cursed } = Opts, State) ->
%     {ok, Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
%     <<ID:32/unsigned-integer, Creation:8/unsigned-integer, Rest3/binary>> = Rest2,
%     IDSeq = lists:reverse([ ID || <<ID:32/unsigned-integer>> <= IDs ]),
%     Format = io_lib:format("#Ref<~B.~B.~B.~B>", [Creation|IDSeq]),
%     RefString = lists:flatten(Format),
%     {ok, list_to_ref(RefString), Rest4};

%---------------------------------------------------------------------
% @todo to be tested
%---------------------------------------------------------------------
% decode(new_reference_ext, <<?NEW_REFERENCE_EXT, Length:16/unsigned-integer, Rest/binary>>
%       ,#{ new_reference_ext := cursed } = Opts, State) ->
%     {ok, _Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
%     <<Creation:8/unsigned-integer, Rest3/binary>> = Rest2,
%     <<IDs:(4*Length)/binary, Rest4/binary>> = Rest3,
%     IDSeq = lists:reverse([ ID || <<ID:32/unsigned-integer>> <= IDs ]),
%     Format = io_lib:format("#Ref<~B.~B.~B.~B>", [Creation|IDSeq]),
%     RefString = lists:flatten(Format),
%     {ok, list_to_ref(RefString), Rest4};
decode(new_reference_ext, <<?NEW_REFERENCE_EXT, Length:16/unsigned-integer, Rest/binary>>
      ,#{ new_reference_ext := Context } = Opts, State) ->
    {raw, Node, Rest2} = decode(atom_ext, Rest, Opts#{ atom_ext => raw }, State),
    <<Creation:8/unsigned-integer, Rest3/binary>> = Rest2,
    <<IDs:(4*Length)/binary, Rest4/binary>> = Rest3,
    case Context of
        drop ->
            {next, Rest4};
        raw ->
            RawTerm = <<Length:16/unsigned-integer, Node/binary
                       ,Creation:8/unsigned-integer, IDs:(4*Length)/binary>>,
            {raw, RawTerm, Rest4};
        enabled ->
            IDSeq = lists:reverse([ ID || <<ID:32/unsigned-integer>> <= IDs ]),
            Format = io_lib:format("#Ref<~B.~B.~B.~B>", [Creation|IDSeq]),
            RefString = lists:flatten(Format),
            {ok, list_to_ref(RefString), Rest4};
        _ ->
            Reason = [{reason, "new_reference_ext disabled"}],
            decode_error(Reason, Opts, State)
    end;

%---------------------------------------------------------------------
% @todo to cleanup this mess
%---------------------------------------------------------------------
% decode(newer_reference_ext, <<?NEWER_REFERENCE_EXT, Length:16/unsigned-integer, Rest/binary>>
%       ,#{ newer_reference_ext := cursed } = Opts, State) ->
%     {ok, _Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
%     <<Creation:32/unsigned-integer, Rest3/binary>> = Rest2,
%     <<IDs:(4*Length)/binary, Rest4/binary>> = Rest3,
%     IDSeq = lists:reverse([ ID || <<ID:32/unsigned-integer>> <= IDs ]),
%     Format = io_lib:format("#Ref<~B.~B.~B.~B>", [Creation|IDSeq]),
%     RefString = lists:flatten(Format),
%     {ok, list_to_ref(RefString), Rest4};
decode(newer_reference_ext, <<?NEWER_REFERENCE_EXT, Length:16/unsigned-integer, Rest/binary>>
      ,#{ newer_reference_ext := Context } = Opts, State) ->
    {raw, Node, Rest2} = decode(atom_ext, Rest, Opts#{ atom_ext => raw }, State),
    <<Creation:32/unsigned-integer, Rest3/binary>> = Rest2,
    <<IDs:(4*Length)/binary, Rest4/binary>> = Rest3,
    case Context of
        drop -> {next, Rest4};
        raw ->
            RawTerm = <<Node/binary, Creation:32/unsigned-integer
                       ,IDs:(4*Length)/binary>>,
            {raw, RawTerm, Rest4};
        {callback, Callback} ->
            RawTerm = <<?NEWER_REFERENCE_EXT, Node/binary
                       , Creation:32/unsigned-integer
                       , IDs:(4*Length)/binary>>,
            decode_callback(Callback, RawTerm, Rest4, Opts, State);
        enabled ->
            IDSeq = lists:reverse([ ID || <<ID:32/unsigned-integer>> <= IDs ]),
            Format = io_lib:format("#Ref<~B.~B.~B.~B>", [Creation|IDSeq]),
            RefString = lists:flatten(Format),
            {ok, list_to_ref(RefString), Rest4};
        _ ->
            Reason = [{reason, "newer_reference_ext disabled"}],
            decode_error(Reason, Opts, State)
    end;

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(small_big_ext, <<?SMALL_BIG_EXT, Size:8/unsigned-integer, Sign:8/unsigned-integer, Rest/binary>>
      , #{ small_big_ext := cursed } = _Opts, _State) ->
    <<Value:Size/binary, Rest2/binary>> = Rest,
    Bignum = ?BINARY_TO_TERM(<<131, ?SMALL_BIG_EXT, Size:8/unsigned-integer
                              ,Sign:8/unsigned-integer, Value:Size/binary>>),
    {ok, Bignum, Rest2};
decode(small_big_ext, <<?SMALL_BIG_EXT, Size:8/unsigned-integer, Sign:8/unsigned-integer, Rest/binary>>
      , #{ small_big_ext := Context } = Opts, State) ->
    <<Value:Size/binary, Rest2/binary>> = Rest,
    case Context of
        drop ->
            {next, Rest2};
        raw ->
            RawTerm = <<Size:8/unsigned-integer, Sign:8/unsigned-integer, Value:Size/binary>>,
            {raw, RawTerm, Rest2};
        {callback, Callback} ->
            RawTerm = <<?SMALL_BIG_EXT, Size:8/unsigned-integer, Sign:8/unsigned-integer, Value:Size/binary>>,
            decode_callback(Callback, RawTerm, Rest2, Opts, State);
        _ ->
            Reason = [{reason, "small_big_ext disabled"}],
            decode_error(Reason, Opts, State)
    end;

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(large_big_ext, <<?LARGE_BIG_EXT, Size:32/unsigned-integer, Sign:8/unsigned-integer, Rest/binary>>
      , #{ large_big_ext := cursed } = _Opts, _State) ->
    <<Value:Size/binary, Rest2/binary>> = Rest,
    Bignum = ?BINARY_TO_TERM(<<131, ?LARGE_BIG_EXT, Size:32/unsigned-integer
                              ,Sign:8/unsigned-integer, Value:Size/binary>>),
    {ok, Bignum, Rest2};
decode(large_big_ext, <<?LARGE_BIG_EXT, Size:32/unsigned-integer, Sign:8/unsigned-integer, Rest/binary>>
      , #{ large_big_ext := Context } = Opts, State) ->
    <<Value:Size/binary, Rest2/binary>> = Rest,
    case Context of
        drop ->
            {next, Rest2};
        raw ->
            RawTerm = <<Size:32/unsigned-integer, Sign:8/unsigned-integer, Value:Size/binary>>,
            {raw, RawTerm, Rest2};
        {callback, Callback} ->
            RawTerm = <<?LARGE_BIG_EXT, Size:32/unsigned-integer, Sign:8/unsigned-integer, Value:Size/binary>>,
            decode_callback(Callback, RawTerm, Rest2, Opts, State);
        _ ->
            Reason = [{reason, "large_big_ext disabled"}],
            decode_error(Reason, Opts, State)
    end;

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(export_ext, <<?EXPORT_EXT, Rest/binary>>, #{ export_ext := enabled } = Opts, _State) ->
    ExportExtOpts = maps:get(export_ext_options, Opts, Opts),
    {ok, Module, RestModule} = decode(atom_ext, Rest, ExportExtOpts, #state{}),
    {ok, Function, RestFunction} = decode(atom_ext, RestModule, ExportExtOpts, #state{}),
    {ok, Arity, RestArity} = decode(small_integer_ext, RestFunction, Opts, #state{}),
    case {is_atom(Module), is_atom(Function)} of
        {true, true} ->
            {ok, fun Module:Function/Arity, RestArity};
        {_,_} ->
            {ok, {export_ext, {Module, Function, Arity}, RestArity}}
    end;

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(new_fun_ext, <<?NEW_FUN_EXT, Size:32/unsigned-integer, Rest/binary>>
      , #{ new_fun_ext := Mode } = Opts, State) ->
    RealSize = Size-4,
    <<Payload:RealSize/binary, Rest2/binary>> = Rest,
    case Mode of
        {callback, Callback} ->
            RawTerm = <<?NEW_FUN_EXT, Size:32/unsigned-integer, Payload:RealSize/binary>>,
            decode_callback(Callback, RawTerm, Rest2, Opts, State);
        cursed ->
            Fun = ?BINARY_TO_TERM(<<131, ?NEW_FUN_EXT,  Size:32/unsigned-integer, Payload:RealSize/binary>>),
            {ok, Fun, Rest2};
        disabled ->
            {error, [{reason, "new_fun_ext disabled"}], State}
    end;

%---------------------------------------------------------------------
% Wildcard Pattern
%---------------------------------------------------------------------
decode(Parser, Rest, Opts, State) ->
    { error
    , [{reason, "unsupporter parser"}
      ,{parser, Parser}
      ,{rest, Rest}
      ,{opts, Opts},
       {state, State}]
    , State}.

decode_test() ->
    [ decode_properties(integer, default_options())
    , decode_properties(atom, default_options())
    , decode_properties(float, default_options())
    , decode_properties(string_ext, default_options())
    , decode_properties(binary, default_options())
    , decode_properties(list, default_options())
    , decode_properties(tuple, default_options())
    , decode_properties(map, default_options())
    ].

%%--------------------------------------------------------------------
%% @hidden
%% @doc function helper for property based testing.
%% @end
%%--------------------------------------------------------------------
property_check(Term, Opts) ->
    property_check('=:=', Term, Opts).

property_check(Operator, Term, Opts) ->
    erlang:Operator({ok, Term}, decode(term_to_binary(Term), Opts)).

%%--------------------------------------------------------------------
%% @hidden
%% @doc properties generator.
%% @end
%%--------------------------------------------------------------------
decode_properties(integer, Opts) ->
    proper:quickcheck(?FORALL( Integer
                             , integer(-4294967296,4294967296)
                             , property_check(Integer, Opts))
                     , 10000);
decode_properties(big, Opts) ->
    proper:quickcheck(?FORALL( Integer
                             , largeinteger()
                             , property_check(Integer, Opts))
                     , 10000);
decode_properties(float, Opts) ->
    proper:quickcheck(?FORALL( Float
                             , float()
                             , property_check(Float,Opts))
                     , 10000);
decode_properties(atom, Opts) ->
    proper:quickcheck(?FORALL(Atom
                             , integer()
                             , property_check(Atom, Opts))
                     , 10000);
decode_properties(string_ext, Opts) ->
    proper:quickcheck(?FORALL( String
                             , string()
                             , property_check(String, Opts))
                     , 10000);
decode_properties(binary, Opts) ->
    proper:quickcheck(?FORALL( Binary
                             , binary()
                             , property_check(Binary, Opts))
                     , 10000);
decode_properties(bitstring, Opts) ->
    proper:quickcheck(?FORALL( Bitstring
                             , bitstring()
                             , property_check(Bitstring, Opts))
                     , 10000);
decode_properties(list, Opts) ->
    Types = [integer(), string(), binary()],
    Tuples = tuple(Types),
    Lists = list(Types),
    Elements = [Lists,Tuples|Types],
    proper:quickcheck(?FORALL( List
                             , list(Elements)
                             , property_check(List, Opts))
                     , 10000);
decode_properties(tuple, Opts) ->
    Types = [integer(), string(), binary()],
    Tuples = tuple(Types),
    Lists = list(Types),
    Elements = [Lists,Tuples|Types],
    proper:quickcheck(?FORALL( Tuple
                             , tuple(Elements)
                             , property_check(Tuple, Opts))
                     , 10000);
decode_properties(map, Opts) ->
    Types = [integer(), string(), binary()],
    Tuples = tuple(Types),
    Lists = list(Types),
    Elements = [Lists,Tuples|Types],
    Maps = map(Elements, Elements),
    Keys = union([integer(), string(), binary(), Tuples, Lists, Maps]),
    Values = union([integer(), string(), binary(), Tuples, Lists, Maps]),
    proper:quickcheck(?FORALL( Map
                             , {Keys, Values}
                             , property_check(Map, Opts)
                             )
                     , 10000).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_atoms(Binary, #{ atoms := {whitelist, Whitelist }})
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
%% @end
%%--------------------------------------------------------------------
decode_tuple_ext(Arity, Rest, Opts, State) ->
    ?LOG_DEBUG("~p", [{decode_tuple_ext, Arity, Opts, State}]),
    decode_tuple_ext(Arity, Rest, Opts, State#state{ depth = State#state.depth+1 }, []).

decode_tuple_ext(0, Rest, _Opts, #{ tuple_ext := disabled } = _State, _Buffer) ->
    {next, Rest};
decode_tuple_ext(0, Rest, _Opts, _State, Buffer) ->
    {ok, list_to_tuple(lists:reverse(Buffer)), Rest};
decode_tuple_ext(Arity, Rest, Opts, State, Buffer) ->
    case decode(Rest, Opts, #state{}) of
        {ok, Term, Rest2} ->
            decode_tuple_ext(Arity-1, Rest2, Opts, State, [Term|Buffer]);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_string_ext(Length, Rest, Opts, State) ->
    ?LOG_DEBUG("~p", [{decode_string_ext, Length, Opts, State}]),
    decode_string_ext(Length, Rest, Opts, State#state{ depth = State#state.depth+1 }, []).

decode_string_ext(0, Rest, #{ string_ext := disabled } = _Opts, _State, _Buffer) ->
    {next, Rest};
decode_string_ext(0, Rest, _Opts, _State, Buffer) ->
    {ok, lists:reverse(Buffer), Rest};
decode_string_ext(Length, <<Character:8/unsigned-integer, Rest/binary>>, Opts, State, Buffer) ->
    decode_string_ext(Length-1, Rest, Opts, State, [Character|Buffer]).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_list_ext(Length, Elements, Opts, State) ->
    ?LOG_DEBUG("~p", [{decode_list_ext, Length, Opts, State}]),
    decode_list_ext(Length, Elements, Opts, State#state{ depth = State#state.depth+1 }, []).

decode_list_ext(0, Rest, Opts, #{ list_ext := disabled } = State, _Buffer) ->
    {ok, [], Rest2} = decode(nil_ext, Rest, Opts, State),
    {next, Rest2};
decode_list_ext(0, Rest, Opts, State, Buffer) ->
    {ok, [], Rest2} = decode(nil_ext, Rest, Opts, State),
    {ok, lists:reverse(Buffer), Rest2};
decode_list_ext(Length, Rest, Opts, State, Buffer) ->
    case decode(Rest, Opts, State) of
        {ok, Term, Rest2} ->
            decode_list_ext(Length-1, Rest2, Opts, State, [Term|Buffer]);
        {next, Rest2} ->
            decode_list_ext(Length-1, Rest2, Opts, State, Buffer);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_map_ext(Arity, Rest, Opts, State) ->
    ?LOG_DEBUG("~p", [{decode_map_ext, Arity, Opts, State}]),
    decode_map_ext(Arity, Rest, Opts, State#state{ depth = State#state.depth+1 }, #{}).

decode_map_ext(0, Rest, #{ decode_map_ext := disabled } = _Opts, _State, _Buffer) ->
    {next, Rest};
decode_map_ext(0, Rest, _Opts, _State, Buffer) ->
    {ok, Buffer, Rest};
decode_map_ext(Arity, Rest, Opts, State, Buffer) ->
    {ok, Key, RestKey} = decode(Rest, Opts, #state{}),
    {ok, Value, RestValue} = decode(RestKey, Opts, #state{}),
    decode_map_ext(Arity-1, RestValue, Opts, State, Buffer#{ Key => Value }).

%%--------------------------------------------------------------------
%% @hidden
%% @doc This function is used to deal with callback
%%
%% @end
%%--------------------------------------------------------------------
-spec decode_callback(Callback, RawTerm, Rest, Opts, State) -> Return when
      Callback :: function()
                | {Module, Function},
      Module :: atom(),
      Function :: atom(),
      RawTerm :: binary(),
      Rest :: binary(),
      Opts :: map(),
      State :: #state{},
      Return :: {ok, Term, Rest}
              | {error, Reason},
      Term :: term(),
      Reason :: term().

decode_callback(Callback, RawTerm, Rest, Opts, State)
  when is_function(Callback) ->
    apply(Callback, [RawTerm, Rest, Opts, State]);
decode_callback({Module, Function}, RawTerm, Rest, Opts, State)
  when is_atom(Module) andalso is_atom(Function) ->
    apply(Module, Function, [RawTerm, Rest, Opts, State]).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_error(Reason, Opts, State) ->
    Error = maps:get(errors, Opts, undefined),
    Message = {error, Reason, State},
    ?LOG_ERROR("~p", [Message]),
    case Error of
        throw -> throw(Error);
        _ -> Message
    end.
