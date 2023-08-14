%%%===================================================================
%%% @copyright Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%===================================================================
-module(berty_bert).
-export([default_options/0]).
-export([decode/1]).

default_options() ->
    #{ small_integer_ext => enabled
     , integer_ext => enabled
     , float_ext => enabled
     , new_float_ext => enabled
     , atom_utf8_ext => disabled
     , atom_ext => disabled
     , small_atom_ext => disabled
     , small_atom_utf8_ext => disabled
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
     }.

decode(Data) ->
    berty_etf:decode(Data, default_options()).

    
