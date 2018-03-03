-module(cpf_funs).

-export([
    do_while/1
]).

-type fun_spec() :: {
    Id :: atom(),
    Fun :: fun(),
    Args :: [term() | {PrevId :: atom()}]
}.

-spec do_while(FunSpecs :: [fun_spec()]) -> Result :: term().
do_while(FunSpecs) ->
    {_Id, Result} = hd(lists:foldl(fun do_while/2, [], FunSpecs)),
    Result.

do_while(_FunSpec, Results = [{_Id, false}|_]) -> Results;
do_while(_FunSpec, Results = [{_Id, error}|_]) -> Results;
do_while(_FunSpec, Results = [{_Id, {error, _Reason}}|_]) -> Results;

do_while({Id, Fun, Args}, Results) ->
    [{Id, apply(Fun, do_while_args(Args, Results))}|Results].

do_while_args(Args, Results) ->
    lists:map(do_while_arg_fun(Results), Args).

do_while_arg_fun(Results) -> fun
    ({Id}) when is_atom(Id) ->
        case lists:keyfind(Id, 1, Results) of
            {Id, {ok, Result}} -> Result;
            {Id, {true, Result}} -> Result;
            {Id, Result} -> Result
        end;
    (Arg) -> Arg
end.
