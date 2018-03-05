# Common purpose functions library

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Provides various functions to simplify some routines.

## cpf_funs

[Erlang Funs](http://erlang.org/doc/programming_examples/funs.html) processing
functions.


### apply_while

Iterates over a list of `fun_spec()` and calls each fun until any of them
returns `error_condition()`. If none of the funs returned error condition the
result of the last called function is returned.

```
-type fun_spec() :: {
    Id :: atom(),
    Fun :: fun(),
    Args :: [{ExistingId :: atom()} | term()]
}.

-type error_condition() :: false | error | {error, Reason}.
```

This example calls three functions to add one to a doubled list sum:

```
cpf_funs:apply_while([
    {sum, fun lists:sum/1, [[1,2,3,4,5]]},
    {double, fun(X) -> 2*X end, [{sum}]},
    {add_one, fun(X) -> X+1 end, [{double}]}
]).
31
```

This example fails on a second call:

```
cpf_funs:apply_while([
    {sum, fun lists:sum/1, [[1,2,3,4,5]]},
    {double, fun(_X) -> {error, not_implemented} end, [{sum}]},
    {add_one, fun(X) -> X+1 end, [{double}]}
]).
{error, not_implemented}
```

As seen from the examples the result returned from any of the Funs can be
retrieved in each step by its by making a one element tuple with its "Id". If a
Fun returns an `ok_condition()`:

```
-type ok_condition() :: {ok, Result} | {true, Result}
```

then requesting the Fun result by "Id" returns the "Result" part only.
