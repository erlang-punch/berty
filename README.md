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

## How really? Is it serious?

In fact, a simple solution already exists, using the option `safe` or
`used` when using
[`binary_to_term/2`](https://www.erlang.org/doc/man/erlang.html#binary_to_term-2). It
will protect you from creating non-existing atoms, but how many
projects are using that?

- [`mojombo/bert.erl`](https://github.com/mojombo/bert.erl):
  https://github.com/mojombo/bert.erl/blob/master/src/bert.erl#L25

  ```erlang
  -spec decode(binary()) -> term().
  
  decode(Bin) ->
    decode_term(binary_to_term(Bin)).
    
  ```

- [`mojombo/ernie`](https://github.com/mojombo/ernie):
  https://github.com/mojombo/ernie/blob/master/elib/ernie_server.erl#L178
  
  ```erlang
  receive_term(Request, State) ->
    Sock = Request#request.sock,
      case gen_tcp:recv(Sock, 0) of
          {ok, BinaryTerm} ->
            logger:debug("Got binary term: ~p~n", [BinaryTerm]),
            Term = binary_to_term(BinaryTerm),
  ```

- [`sync/n2o`](https://github.com/synrc/n2o):
  https://github.com/synrc/n2o/blob/master/src/services/n2o_bert.erl#L8
  
  ```erlang
  encode(#ftp{}=FTP) -> term_to_binary(setelement(1,FTP,ftpack));
  encode(Term)       -> term_to_binary(Term).
  decode(Bin)        -> binary_to_term(Bin).
  ```

- [`ferd/bertconf`](https://github.com/ferd/bertconf):
  https://github.com/ferd/bertconf/blob/master/src/bertconf_lib.erl#L10

  ```erlang
  decode(Bin) ->
      try validate(binary_to_term(Bin)) of
        Terms -> {ok, Terms}
      catch
        throw:Reason -> {error, Reason}
      end.
  ```

- [`a13x/aberth`](https://github.com/a13x/aberth):
  https://github.com/a13x/aberth/blob/master/src/bert.erl#L25
  
  ```erlang
  -spec decode(binary()) -> term().
  
  decode(Bin) ->
    decode_term(binary_to_term(Bin)).
  ```
  

- [`yuce/bert.erl`](https://github.com/yuce/bert.erl):
  https://github.com/yuce/bert.erl/blob/master/src/bert.erl#L24

  ```erlang
  -spec decode(binary()) -> term().
  decode(Bin) ->
      decode_term(binary_to_term(Bin)).
  ```

- And probably many more like this search on
  [`searchcode.com`](https://searchcode.com/?lan=25&q=binary_to_term)
  or
  [`github.com`](https://github.com/search?q=binary_to_term+language%3AErlang&type=code&l=Erlang)
  suggest.

It's highly probable lot of those functions are hard to call, but it
could be the case. In situation where unknown data are coming,
`erlang:binary_to_term/1` and even `erlang:binary_to_term/2` should be
avoided or carefully used.

## Why am I not aware of that?

Few articles[^erlef-atom-exhaustion][^paraxial-atom-dos] have been
created in the past to explain these problems.

[^erlef-atom-exhaustion]: https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/atom_exhaustion.html
[^paraxial-atom-dos]: https://paraxial.io/blog/atom-dos

## How to fix the root cause?

The problem is from atoms, at least one
paper[^atom-garbage-collection] talked about that. Fixing the garbage
collection issue could help a lot, but if it's not possible for many
reason, using an high level implementation of ETF with some way to
control what kind of data are coming might be an "okayish" solution.

[^atom-garbage-collection]: Atom garbage collection by Thomas Lindgren, https://dl.acm.org/doi/10.1145/1088361.1088369
