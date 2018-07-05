-module(cpf_funs).

-export([
    apply_while/1,
    apply_on/2, apply_on/3
]).

-type fun_spec() :: {
    Id :: atom(),
    Fun :: fun(),
    Args :: [term() | {PrevId :: atom()}]
}.

-spec apply_while(FunSpecs :: [fun_spec()]) -> Result :: term().
apply_while(FunSpecs) ->
    {_Id, Result} = hd(lists:foldl(fun apply_while/2, [], FunSpecs)),
    Result.

apply_while(_FunSpec, Results = [{_Id, false}|_]) -> Results;
apply_while(_FunSpec, Results = [{_Id, error}|_]) -> Results;
apply_while(_FunSpec, Results = [{_Id, {error, _Reason}}|_]) -> Results;

apply_while({Id, Fun, Args}, Results) ->
    [{Id, apply(Fun, apply_while_args(Args, Results))}|Results].

apply_while_args(Args, Results) ->
    lists:map(apply_while_arg_fun(Results), Args).

apply_while_arg_fun(Results) -> fun
    ({Id}) when is_atom(Id) ->
        case lists:keyfind(Id, 1, Results) of
            {Id, {ok, Result}} -> Result;
            {Id, {true, Result}} -> Result;
            {Id, Result} -> Result
        end;
    (Arg) -> Arg
end.

-spec apply_on(Result :: {Status :: term(), Data :: term()}, Fun :: fun()) ->
    ModifiedResult :: {Status :: term(), ModifiedData :: term()}.

apply_on({Status, Data}, Fun) -> {Status, Fun(Data)}.

-spec apply_on(Status :: term(),
               Result :: {Status :: term(), Data :: term()},
               Fun :: fun()
) ->
    ModifiedResult :: {Status :: term(), ModifiedData :: term()}.

apply_on(Status, {Status, Data}, Fun) -> {Status, Fun(Data)};
apply_on(_Status, Result, _Fun) -> Result.
