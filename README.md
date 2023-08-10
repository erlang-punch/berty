# berty

A clean, safe and flexible implementation of BERT, a data-structure
format inspired by Erlang ETF.


## Build

    $ rebar3 compile

# FAQ

## Why creating another BERT implementation?

Mainly because of atoms management. In fact, `binary_to_term/1` and
`term_to_binary/1` are not safe, if unknown data are coming from
untrusted source, it's quite easy to simply kill the node by
overflowing the number of atoms managed by the node itself, and
probably also a full cluster if this data is shared.

```erlang
% first erlang shell
file:write_file("atom1", term_to_binary([ list_to_atom("$test-" ++ integer_to_list(X)) || X <- lists:seq(1,1_000_000) ])).
% second erlang shell
file:write_file("atom2", term_to_binary([ list_to_atom("$test-" ++ integer_to_list(X)) || X <- lists:seq(1_000_000,2_000_000) ])).
```

Now restore those 2 files on another node.

```erlang
% third erlang shell
f(D), {ok, D} = file:read_file("atom1"), binary_to_term(D).
f(D), {ok, D} = file:read_file("atom2"), binary_to_term(D).
no more index entries in atom_tab (max=1048576)

Crash dump is being written to: erl_crash.dump...done
```

Doh. Erlang VM crashed. We can fix that in many different way, here
few examples:

 - avoid using `binary_to_term/1` and `term_to_binary/1` functions,
   instead create our own parser based on ETF specification. When
   terms are deserialized, atoms can be (1) converted in existing atom
   (2) converted in binary or list (3) simply dropped or replaced with
   something to alert the VM this part of the data is dangerous.
   
 - keep our own local atom table containing all atom deserialized. A
   soft/hard limit can be set.
