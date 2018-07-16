# Common purpose functions library

[![License: MIT][MIT badge]][MIT]

Provides various functions to simplify some routines.

## cpf_funs

[Erlang Funs] processing functions.

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

### apply_on

Applies a function on a second element of tuple if the first element matches
the specified parameter. Consider the examples:

```
% Example 1:
case function() of
    {ok, Data} -> {ok, handle_data(Data)};
    {error, Reason} -> {error, Reason}
end,

% Example 2:
case function() of
    {ok, Data} -> {ok, Data};
    {error, Reason} -> {error, handle_error(Reason)}
end.
```

it could be rewritten this way:

```
% Example 1:
apply_on(ok, function(), fun handle_data/1),

% Example 2:
apply_on(error, function(), fun handle_error/1).
```

To apply a Fun on either of results:

```
apply_on(function(), fun handle_anything/1).
```

## cpf_env

Provides ability to set [Erlang Application Env] from a file or binary data.

### read_files

Reads configuration from one of a specified list of files.

Assuming the file "exists.config" has the following content (note the dot at the
end):

```
[{app1, [
    {k11, v11},
    {k12, v12}
]},
{app2, [
    {k21, v21},
    {k22, v22}
]}].
```

Then it could be specified in a list of the other files:

```
cpf_env:read_files([
    "does_not_exist1.config",
    "exists.config",
    "does_not_exist2.config"
]).
{ok,{{"exists.config",
      [{app1,[{k11,v11},{k12,v12}]},
       {app2,[{k21,v21},{k22,v22}]}]},
     [{"does_not_exist1.config",enoent}]}}
```

The result contains succesfully read configuration from the second file and
the reason why the first file read failed (in reverse order). There was no
attempt to read the third file, as read of the second one was succesful before.

### read_file

Can be used to read configuration from one exact file.

### read_binary

Reads configuration from a binary (note the dot):

```
cpf_env:load_binary(<<"[{app1, [{k1, v1}]}, {app2, [{k2, v2}]}].">>).
{ok,[{app1,[{k1,v1}]},{app2,[{k2,v2}]}]}
```

### set

Read configuration can be set as a corresponding application env:

```
{ok, {{File, Config}, Errors}} = cpf_env:read_file("exists.config").
{ok,[{app1,[{k11,v11},{k12,v12}]},
     {app2,[{k21,v21},{k22,v22}]}]}

cpf_env:set(Config).
ok
```

The applications "app1" and "app2" env will be set accordingly.

### load

Reads configuration from binary, file or one of a list of files and sets
application env if read was successful.

## cpf_cmd

A module for running operating system executables.

### run

Runs an OS executable:

```
cpf_cmd:run("date").
{ok,"Mon Jul 16 12:10:17 CEST 2018\n"}
```

with args:

```
cpf_cmd:run({"date ~s", ["-u"]}).
{ok,"Mon Jul 16 10:12:04 UTC 2018\n"}
```

with options:

```
cpf_cmd:run("ls -1", [{cd, "/usr/local/Cellar/erlang"}]).
{ok,"20.1.3\n20.2.2\n21.0\n"}

cpf_cmd:run("echo -n $CPF_CMD").
ok

cpf_cmd:run("echo -n $CPF_CMD", [{env, [{"CPF_CMD", "RUN!"}]}]).
{ok,"RUN!"}
```

## cpf_node

Starts or stops an Erlang distribution node.

### start

Start a node ensuring [Epmd] is running:

```
cpf_node:start(my_project).
ok
```

Stop a running node (will not stop running Epmd):

```
cpf_node:stop().
ok
```

<!-- Links -->
[MIT]: https://opensource.org/licenses/MIT
[Erlang Funs]: http://erlang.org/doc/programming_examples/funs.html
[Erlang Application Env]: http://erlang.org/doc/apps/kernel/application.html#set_env-3
[Epmd]: http://erlang.org/doc/man/epmd.html

<!-- Badges -->
[MIT badge]: https://img.shields.io/badge/License-MIT-yellow.svg?style=flat-square
