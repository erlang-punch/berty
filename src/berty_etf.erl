%%%===================================================================
%%% @copyright Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%===================================================================
-module(berty_etf).
-export([default_options/0]).
-export([decode/1, decode/2]).
-include("berty.hrl").
-include_lib("kernel/include/logger.hrl").
-record(state, { id = make_ref()
               , module = ?MODULE
               , started_at = undefined
               , ended_at = undefined
               , depth = 0
               , metric_process = berty_metrics
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

default_options() ->
    #{ small_integer_ext => enabled
     , integer_ext => enabled
     , float_ext => enabled
     , new_float_ext => enabled
     , atom_utf8_ext => enabled
     , atom_ext => enabled
     , small_atom_ext => enabled
     , small_atom_utf8_ext => enabled
     , small_tuple_ext => enabled
     , small_tuple_ext_arity => {0, 255}
     , large_tuple_ext => enabled
     , large_tuple_ext_arity => {0, 1024}
     , string_ext => enabled
     , string_ext_length => {0, 65535}
     , nil_ext => enabled
     , list_ext => enabled
     , list_ext_length => {0, 1048576}
     , binary_ext => enabled
     , bit_binary_ext => enabled
     , map_ext => enabled
     , map_ext_length => {0, 65535}
     % , map_ext_key_terms => [binary_ext, string_ext, ...]
     % , map_ext_value_terms => [any]
     }.

-type callback_option() :: {callback, function() | atom()}.
-type limit_option() :: {number(), number()}.
-type small_integer_ext_option() :: enabled | disabled | callback_option().
-type small_integer_ext_limit_option() :: limit_option().
-type integer_ext_option() ::  enabled | disabled | callback_option().
-type integer_ext_limit_option() :: limit_option().
-type float_ext_option() ::  enabled | disabled | cursed | callback_option().
-type float_ext_limit_option() :: limit_option().
-type new_float_ext_option() ::  enabled | disabled | cursed | callback_option().
-type new_float_ext_limit_option() :: limit_option().
-type atom_ext_option() :: enabled | disabled | cursed | callback_option().
-type atom_ext_size_option() :: limit_option().
-type small_atom_ext_option() ::  enabled | disabled | cursed | callback_option().
-type small_atom_ext_size_option() :: limit_option().
-type atoms_option() :: create 
                      | {create, number()} 
                      | {create, number(), warning}
                      | as_string
                      | as_binary
                      | existing.

-type options() :: #{ small_integer_ext => small_integer_ext_option()
                    , small_integer_ext_limit => small_integer_ext_limit_option()
                    , integer_ext => integer_ext_option()
                    , integer_ext_limit => integer_ext_limit_option()
                    , float_ext => float_ext_option()
                    , float_ext_limit => float_ext_limit_option()
                    , new_float_ext => new_float_ext_option()
                    , new_float_ext_limit => new_float_ext_limit_option()
                    , atom_ext => atom_ext_option()
                    , atom_ext_size => atom_ext_size_option()
                    , small_atom_ext => small_atom_ext_option()
                    , small_atom_ext_size => small_atom_ext_size_option()
                    , atoms => atoms_option()
                    }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec decode(Data) -> Return when
      Data :: binary(),
      Return :: {ok, term()} 
              | {ok, term(), binary()} 
              | {error, Reason},
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
              | {error, Reason},
      Opts :: options(),
      Reason :: proplists:proplist().

decode(<<131, Data/binary>> = _Payload, Opts) ->
    State = #state{ started_at = erlang:monotonic_time() },
    DefaultOpts = default_options(),
    NewOpts = maps:merge(DefaultOpts, Opts),
    case decode(Data, NewOpts, State) of
        {ok, Term, <<>>} -> {ok, Term};
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

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%
%% depth => {Min, Max}
%% size => {Min, Max}
%%--------------------------------------------------------------------

%---------------------------------------------------------------------
% small_integer_ext => enabled | disabled | {callback, Callback}
% small_integer_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(small_integer_ext, <<?SMALL_INTEGER_EXT, Integer/signed-integer, Rest/binary>>, #{ small_integer_ext := cursed }, _State) ->
    Result = ?BINARY_TO_TERM(<<131, ?SMALL_INTEGER_EXT, Integer/integer>>),
    {ok, Result, Rest};
decode(small_integer_ext, <<?SMALL_INTEGER_EXT, Integer/signed-integer, Rest/binary>>, _, _State) ->
    {ok, Integer, Rest};

%---------------------------------------------------------------------
% integer_ext => enabled | disabled | {callback, Callback}
% integer_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(integer_ext, <<?INTEGER_EXT, Integer:32/signed-integer, Rest/binary>>, _, _State) ->
    {ok, Integer, Rest};

%---------------------------------------------------------------------
% float_ext => enabled | disabled | cursed | {callback, Callback}
% float_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(float_ext, <<?FLOAT_EXT, Float:31/binary, Rest/binary>>, #{ minor_version := 0, float_ext := cursed }, _State) ->
    Result = ?BINARY_TO_TERM(<<131, ?FLOAT_EXT, Float:31/binary>>),
    {ok, Result, Rest};
decode(float_ext, <<?FLOAT_EXT, Float:31/binary, Rest/binary>>, #{ minor_version := 0 }, _State) ->
    NewFloat = binary_to_float(Float),
    {ok, NewFloat, Rest};

%---------------------------------------------------------------------
% new_float_ext => enabled | disabled | cursed | {callback, Callback}
% new_float_ext_limit => {Min, Max}
%---------------------------------------------------------------------
decode(new_float_ext, <<?NEW_FLOAT_EXT, Float:8/binary, Rest/binary>>, #{ new_float_ext := cursed }, _State) ->
    Result = ?BINARY_TO_TERM(<<131, ?NEW_FLOAT_EXT, Float/binary>>),
    {ok, Result, Rest};

%---------------------------------------------------------------------
% atom_ext => enabled | disabled | cursed | {callback, Callback}
% atom_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(atom_ext, <<?ATOM_EXT, Length:16/unsigned-integer, Rest/binary>>, #{ atom_ext := disabled }, _State) ->
    <<_Atom:Length/binary, Rest2/binary>> = Rest,
    {next, Rest2};
decode(atom_ext, <<?ATOM_EXT, Length:16/unsigned-integer, Rest/binary>>, #{ atom_ext := cursed }, _State) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = ?BINARY_TO_TERM(<<131, ?ATOM_EXT, Length:16/unsigned-integer, Atom:Length/binary>>),
    {ok, NewAtom, Rest2};
decode(atom_ext, <<?ATOM_EXT, Length:16/unsigned-integer, Rest/binary>>, Opts, _State) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = decode_atoms(Atom, Opts),
    {ok, NewAtom, Rest2};

%---------------------------------------------------------------------
% small_atom_ext => enabled | disabled | cursed | {callback, Callback}
% small_atom_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(small_atom_ext, <<?SMALL_ATOM_EXT, Length:8/unsigned-integer, Rest/binary>>, #{ small_atom_ext := disabled }, _State) ->
    <<_Atom:Length, Rest2/binary>> = Rest,
    {next, Rest2};
decode(small_atom_ext, <<?SMALL_ATOM_EXT, Length:8/unsigned-integer, Rest/binary>>, #{ small_atom_ext := cursed }, _State) ->
    <<Atom:Length, Rest2/binary>> = Rest,
    NewAtom = ?BINARY_TO_TERM(<<131, ?SMALL_ATOM_EXT, Length:8/unsigned-integer, Atom:Length/binary>>),
    {ok, NewAtom, Rest2};
decode(small_atom_ext, <<?SMALL_ATOM_EXT, Length:8/unsigned-integer, Rest/binary>>, Opts, _State) ->
    <<Atom:Length, Rest2/binary>> = Rest,
    NewAtom = decode_atoms(Atom, Opts),
    {ok, NewAtom, Rest2};

%---------------------------------------------------------------------
% small_atom_utf8_ext => enabled | disabled | cursed | {callback, Callback}
% small_atom_utf8_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(small_atom_utf8_ext, <<?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Rest/binary>>, #{ small_atom_utf8_ext := disabled }, _State) ->
    <<_Atom:Length/binary, Rest2/binary>> = Rest,
    {next, Rest2};
decode(small_atom_utf8_ext, <<?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Rest/binary>>, #{ small_atom_utf8_ext := cursed }, _State) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = ?BINARY_TO_TERM(<<131, ?SMALL_ATOM_UTF8_EXT, Length:8/unsigned-integer, Atom:Length/binary>>),
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
decode(atom_utf8_ext, <<?ATOM_UTF8_EXT, Length:16/unsigned-integer, Rest/binary>>, #{ atom_utf8_ext := disabled }, _State) ->
    <<_Atom:Length/binary, Rest2/binary>> = Rest,
    {next, Rest2};
decode(atom_utf8_ext, <<?ATOM_UTF8_EXT, Length:16/unsigned-integer, Rest/binary>>, #{ atom_utf8_ext := cursed }, _State) ->
    <<Atom:Length/binary, Rest2/binary>> = Rest,
    NewAtom = ?BINARY_TO_TERM(<<131, ?ATOM_UTF8_EXT, Length:16/unsigned-integer, Atom:Length/binary>>),
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
decode(small_tuple_ext, <<?SMALL_TUPLE_EXT, 0/unsigned-integer, Rest/binary>>, _, _State) ->
    {ok, {}, Rest};
decode(small_tuple_ext, <<?SMALL_TUPLE_EXT, Arity/unsigned-integer, Rest/binary>>, Opts, State) ->
    decode_tuple_ext(Arity, Rest, Opts, State);

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
decode(binary_ext, <<?BINARY_EXT, Size:32/unsigned-integer, Rest/binary>>, #{ binary_ext := cursed } = _Opts, _State) ->
    <<Binary:Size/binary, Rest2/binary>> = Rest,
    CursedBinary = ?BINARY_TO_TERM(<<131, ?BINARY_EXT, Size:32/unsigned-integer, Binary:Size/binary>>),
    {ok, CursedBinary, Rest2};
decode(binary_ext, <<?BINARY_EXT, Size:32/unsigned-integer, Rest/binary>>, _Opts, _State) ->
    <<Binary:Size/binary, Rest2/binary>> = Rest,
    {ok, Binary, Rest2};

%---------------------------------------------------------------------
% bit_binary_ext => enabled | disabled | {callback, Callback}
% bit_binary_ext_size => {Min, Max}
%---------------------------------------------------------------------
decode(bit_binary_ext, <<?BIT_BINARY_EXT, Size:32/unsigned-integer, Bits:8, Rest/binary>>, #{ bit_binary_ext := cursed } = _Opts, _State) ->
    <<Binary:(Size-1)/binary, Byte:1/binary, Rest2/binary>> = Rest,
    Cursed = <<131, ?BIT_BINARY_EXT, Size:32/unsigned-integer, Bits:8, Binary:(Size-1)/binary, Byte:1/binary>>,
    CursedBits = ?BINARY_TO_TERM(Cursed),
    {ok, CursedBits, Rest2};
decode(bit_binary_ext, <<?BIT_BINARY_EXT, Size:32/unsigned-integer, Bits:8, Rest/binary>>, _Opts, _State) ->
    <<Binary:(Size-1)/binary, Byte:1/binary, Rest2/binary>> = Rest,
    {ok, <<Binary/binary, Byte:Bits/bitstring>>, Rest2};

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

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(new_port_ext, <<?NEW_PORT_EXT, Rest/binary>>, #{ new_port_ext := cursed } = Opts, State) ->
    {ok, _Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
    <<Id:32/unsigned-integer, Creation:32/unsigned-integer, Rest3/binary>> = Rest2,
    Format = io_lib:format("#Port<~B.~B>", [Creation,Id]),
    StringPort = lists:flatten(Format),
    {ok, list_to_port(StringPort), Rest3};

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(v4_port_ext, <<?V4_PORT_EXT, Rest/binary>>, #{ v4_port_ext := cursed } = Opts, State) ->
    {ok, _Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
    <<Id:64/unsigned-integer, Creation:32/unsigned-integer, Rest3/binary>> = Rest2,
    Format = io_lib:format("#Port<~B.~B>", [Creation,Id]),
    StringPort = lists:flatten(Format),
    {ok, list_to_port(StringPort), Rest3};

%---------------------------------------------------------------------
%
%---------------------------------------------------------------------
decode(pid_ext, <<?PID_EXT, Rest/binary>>, #{ pid_ext := cursed } = Opts, State) ->
    {ok, _Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
    <<ID:32/unsigned-integer, Serial:32/unsigned-integer
     ,Creation:8/unsigned-integer, Rest3/binary>> = Rest2,
    Pid = c:pid(Creation, ID, Serial),
    {ok, Pid, Rest3};

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

%---------------------------------------------------------------------
% @todo to be tested
%---------------------------------------------------------------------
decode(reference_ext, <<?REFERENCE_EXT, Rest/binary>>
      ,#{ reference_ext := cursed } = Opts, State) ->
    {ok, Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
    <<ID:32/unsigned-integer, Creation:8/unsigned-integer, Rest3/binary>> = Rest2,
    {error, Node, ID, Creation};

%---------------------------------------------------------------------
% @todo to be tested
%---------------------------------------------------------------------
decode(new_reference_ext, <<?NEW_REFERENCE_EXT, Length:16/unsigned-integer, Rest/binary>>
      ,#{ new_reference_ext := cursed } = Opts, State) ->
    {ok, Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
    <<Creation:8/unsigned-integer, Rest3/binary>> = Rest2,
    <<IDs:(4*Length)/binary, Rest4/binary>> = Rest3,
    IDSeq = lists:reverse([ ID || <<ID:32/unsigned-integer>> <= IDs ]),
    Format = io_lib:format("#Ref<~B.~B.~B.~B>", [Creation|IDSeq]),
    RefString = lists:flatten(Format),
    {ok, list_to_ref(RefString), Rest4};


%---------------------------------------------------------------------
% @todo to cleanup this mess
%---------------------------------------------------------------------
decode(newer_reference_ext, <<?NEWER_REFERENCE_EXT, Length:16/unsigned-integer, Rest/binary>>
      ,#{ newer_reference_ext := cursed } = Opts, State) ->
    {ok, Node, Rest2} = decode(atom_ext, Rest, Opts#{ atoms => create }, State),
    <<Creation:32/unsigned-integer, Rest3/binary>> = Rest2,
    <<IDs:(4*Length)/binary, Rest4/binary>> = Rest3,
    IDSeq = lists:reverse([ ID || <<ID:32/unsigned-integer>> <= IDs ]),
    Format = io_lib:format("#Ref<~B.~B.~B.~B>", [Creation|IDSeq]),
    RefString = lists:flatten(Format),
    {ok, list_to_ref(RefString), Rest4};

%---------------------------------------------------------------------
% Wildcard Pattern
%---------------------------------------------------------------------
decode(Parser, Rest, Opts, State) ->
    {error, [{reason, "unsupporter parser"}
            ,{parser, Parser}
            ,{rest, Rest}
            ,{opts, Opts},
             {state, State}]}.

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
    {ok, Term, Rest2} = decode(Rest, Opts, #state{}),
    decode_tuple_ext(Arity-1, Rest2, Opts, State, [Term|Buffer]).

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
    {ok, Term, Rest2} = decode(Rest, Opts, State),
    decode_list_ext(Length-1, Rest2, Opts, State, [Term|Buffer]).

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
