# Future implementations and Ideas

## ETF Data Description

Describe a term or ETF encoded data to show what kind of terms are
present in it.

```erlang
{ok, {list_ext, 3, [ small_integer_ext
                   , small_integer_ext
                   , small_integer_ext]
                   }}
  = berty:describe([1,2,3]).

{ok, {tuple_ext, 2, [ atom_ext
                    , string_ext
                    ]}}
  = berty:describe({ok, "test"}).
```

## ETF Data Analysis

Take a list of terms or ETF encoded data to analyze them by creating a
summary of the different kind of terms present in them.

```
{ok, {tuple, 2, [[{atom_ext, 1},{atom_ext, 1}]
                ,[{integer_ext, 1},{string_ext, 1}
                ]]}}
  = berty:analyze([{ok, 123},{error,"test}]).
```

## ETF Path

An easy way to extract only one term, similar to xmlpath or jsonpath.

## ETF Schema

A schema to validate ETF data received.
