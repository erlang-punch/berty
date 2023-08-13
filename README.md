# berty

A clean, safe and flexible implementation of BERT, a data-structure
format inspired by Erlang ETF.

## Build

```sh
rebar3 compile
rebar3 shell
```

## Test

```sh
rebar3 as test eunit
rebar3 as test shell
```

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
created in the past to explain these problems. On my side, if I was in
charge of fixing this issue, I would probably do something in two
times.

In the first step, I would probably create a workaround on atom
creation function, with a soft/hard limit. When we reach the soft
limit, warnings are displayed saying we reached the soft limit, but we
can still create new atoms. When reaching the hard limit, atoms can't
be created anymore, and exceptions are raised instead of crashing the
host.

In a second step, I would probably create a flexible interface to
deal with atoms and divide the problem in half:

 1. create fixed atom store containing only atoms from source code
    (Erlang release and project), this one can't be increased.
    
 2. create a second atom store containing dynamically created atoms
    during runtime, this one can be increased.
    
What I worry about is when dealing with mnesia. What could happen if
someone create more than 2M unwanted atoms added in Mnesia or DETS?
What kind of behavior the cluster will have? And how to fix that if
it's critical.
    
Unfortunately, I think it will totally break atom performance, but it
could be an interesting project to learn how Erlang BEAM works under
the hood.

[^erlef-atom-exhaustion]: https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/atom_exhaustion.html
[^paraxial-atom-dos]: https://paraxial.io/blog/atom-dos

## Are atoms the only issue there?

Well, it depends. If you are receving a (very) long string or list
containing terms, it will have a direct impact on the memory, and it
will eventually lead to memory exhaustion:

```erlang
% size of the list should be checked
% if not, memory exhaustion can happen
[ $1 || _ <- lists:seq(0,160_000_000) ].
% eheap_alloc: Cannot allocate 3936326656 bytes of memory (of type "heap").
% Crash dump is being written to: erl_crash.dump...
```

Same behavior can be generated using binaries:

```erlang
% big binaries can crash the BEAM
binary_to_term(<<131, 111, 4294967294:32/unsigned-integer, 0:8/integer, 255:8, 0:4294967280/unsigned-integer>>).
% binary_alloc: Cannot allocate 4294967293 bytes of memory (of type "binary").
% Crash dump is being written to: erl_crash.dump...
```

Finally, generating ETF payload with very long binaries can also have
an impact on CPUs, the following code can generate DoS and if many process 

```erlang
% big payload, high cpu usage, no crash.
% size of the big integer must be checked
% size: 2**18-1, binary byte size: 262_150 (~262kB)
_ = binary_to_term(<<131, 111, 262_143:32/unsigned-integer, 0:8/integer, 255:2_097_144/unsigned-integer>>).

% size: 2**19-1, binary byte size: 524_294 (~524kB)
_ = binary_to_term(<<131, 111, 524_287:32/unsigned-integer, 0:8/integer, 255:4_194_296/unsigned-integer>>).

% size: 2**20-1, binary byte size: 1_048_582 (~1MB)
_ = binary_to_term(<<131, 111, 1_048_575:32/unsigned-integer, 0:8/integer, 255:8_388_600/unsigned-integer>>).
```

It's highly probable other terms can have a deadly impact on a node or
a cluster.

## How to fix the root cause?

The problem is from atoms, at least one
paper[^atom-garbage-collection] talked about that. Fixing the garbage
collection issue could help a lot, but if it's not possible for many
reason, using an high level implementation of ETF with some way to
control what kind of data are coming might be an "okayish" solution.

The "Let it crash" philosophy is quite nice when developing high level
application interacting in a safe place but this philosophy can't be
applied in a place where uncontrolled data is coming. Some functions,
like `binary_to_term/1` must be avoid at all cost.

[^atom-garbage-collection]: Atom garbage collection by Thomas Lindgren, https://dl.acm.org/doi/10.1145/1088361.1088369

## What about ETF schema?

This answer is a draft, a sandbox to design an Erlang ETF Schema
feature.

It might be great to have syntax to create ETF schema, a bit like
protobuf[^protobuf], json schema[^json-schema], XML[^xml] (with
XLST[^xlst]) or ASN.1[^asn.1].

```erlang
schema1() ->
  integer().
  
schema2() ->
  tuple([[atom(ok), integer()]
        ,[atom(error), string(1024)]).
        
% fun ({ok, X}) when is_integer(X) -> true;
%     ({error, X) when is_list(X) andalso length(X) =< 1024 -> is_string(X);
%     (_) -> false.
        
schema3() ->
  tuple(
```

Here the final representation.

```erlang
[{tuple, [{atom, [ok]}, {integer, []}]}
,{tuple, [{atom, [error]}, {string, [1024]}]}
]
% or
[[tuple, [2]]
,[atom, [ok,error]]
,[integer, []]
,[string, [1024]]
].
```

[^protobuf]: https://protobuf.dev/overview/
[^json-schema]: https://json-schema.org/
[^xml]: https://en.wikipedia.org/wiki/XML
[^xlst]: https://en.wikipedia.org/wiki/XSLT
[^asn.1]: https://en.wikipedia.org/wiki/ASN.1

## What about an ETF path feature?

Another feature like xmlpath or jsonpath is also required as well, an
easy syntax and comprehensible one needs to be created. I would like
to include:

 1. pattern matching

```erlang
% how to create an etf path?
% first example
% ETF = #{ key => #{ key2 => { ok, "test"} } }.
"test" = path(ETF, "#key#key2{ok,@}")

% second example
% ETF = [{ok, "test"}, {error, badarg}, {ok, "data"}].
[{ok, "test"},{ok, "data"}] = path(ETF, "[{ok,_}]")
% or
[]{ok,_}

% third example
% ETF = {ok, #{ <<"data">> => [<<"test">>] }}.
[<<"test">>] = path(ETF, "{ok,@}#!data").
```

## Nothing to add?

When I wrote [Serialization series — Do you speak Erlang ETF or BERT?
(part
1)](https://medium.com/@niamtokik/serialization-series-do-you-speak-erlang-etf-or-bert-part-1-ff70096b50c0)
in 2017, someone told me to check another project called
[`jem.js`](https://github.com/inaka/jem.js) and read [Replacing JSON
when talking to Erlang](http://inaka.net/blog/2016/08/17/why-json/)
([archive](https://web.archive.org/web/20180301221900/http://inaka.net/blog/2016/08/17/why-json/))
blog post. What's funny here... Is that:

```erlang
handle_post(Req, State) ->
  {ok, Body, Req1} = cowboy_req:body(Req),
  Decoded = erlang:binary_to_term(Body),
  Reply = do_whatever(Decoded),
  {erlang:term_to_binary(Reply), Req1, State}.
```

Yes, "Faster and more efficient", but can destroy your whole platform
in few second. Don't do that. Please. Unfortunately,
[inaka.net](inaka.net) seems to be down, it would have been funny to
play with that.

## Is there a "risk analysis" for each terms somewhere?

Probably, but I did not find a lot on that. Here a short summary of
each terms is it safe or not and with the risk(s).

| Terms                 | Code |    Safe? | Risks
|:----------------------|-----:|---------:|--------------------------|
| `ATOM_CACHE_REF`      |   82 |       no | atom exhaustion
| `ATOM_EXT`            |  100 |       no | atom exhaustion
| `ATOM_UTF8_EXT`       |  118 |       no | atom exhaustion
| `BINARY_EXT`          |  109 |    maybe | dynamic binary length (32bits)
| `BIT_BINARY_EXT`      |   77 |    maybe | dynamic bitstring length (32bits)
| `EXPORT_EXT`          |  113 |       no | atom exhaustion
| `FLOAT_EXT`           |   99 |      yes | 31 bytes float fixed length
| `FUN_EXT`             |  117 |       no | atoms exhaution
| `INTEGER_EXT`         |   98 |      yes | 1 byte fixed length
| `LARGE_BIG_EXT`       |  111 |    maybe | dynamic integer length (32bits)
| `LARGE_TUPLE_EXT`     |  105 |    maybe | dynamic tuple length (32bits)
| `LIST_EXT`            |  108 |    maybe | dynamic list length (32bits)
| `LOCAL_EXT`           |  121 |      yes | atom exhaustion
| `MAP_EXT`             |  116 |    maybe | dynamic pair length (32bits)
| `NEWER_REFERENCE_EXT` |   90 |       no | memory exhaustion
| `NEW_FLOAT_EXT`       |   70 |      yes | 8 bytes fixed float
| `NEW_FUN_EXT`         |  112 |       no | atom exhaution
| `NEW_PID_EXT`         |   88 |       no | atom exhaution
| `NEW_PORT_EXT`        |   89 |       no | atom exhaution
| `NEW_REFERENCE_EXT`   |  114 |    maybe | dynamic reference length (16bits)
| `NIL_EXT`             |  106 |      yes | fixed length
| `PID_EXT`             |  103 |       no | atom exhaustion
| `PORT_EXT`            |  102 |       no | atom exhaustion
| `REFERENCE_EXT`       |  101 |       no | atom exhaustion
| `SMALL_ATOM_EXT`      |  115 |       no | atom exhaustion
| `SMALL_ATOM_UTF8_EXT` |  119 |       no | atom exhaustion
| `SMALL_BIG_EXT`       |  110 |    maybe | dynamic integer length (8bits)
| `SMALL_INTEGER_EXT`   |   97 |      yes | fixed size
| `SMALL_TUPLE_EXT`     |  104 |    maybe | dynamic tuple length (8bits)
| `STRING_EXT`          |  107 |    maybe | dynamic string length (16bits)
| `V4_PORT_EXT`         |  120 |       no | atom exhaustion
