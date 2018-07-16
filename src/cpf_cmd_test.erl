-module(cpf_cmd_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, cpf_cmd).

run_true_test() ->
    ?assertEqual(ok, ?M:run("true")).

run_false_test() ->
    ?assertEqual({error,{1,[]}}, ?M:run("false")).

run_wrong_test() ->
    {error, {Code, _Details}} = ?M:run("wrong"),
    ?assertEqual(127, Code).

run_echo_env_test() ->
    Result = ?M:run("echo -n $CPF_CMD", [{env, [{"CPF_CMD", "RUN!"}]}]),
    ?assertEqual({ok,"RUN!"}, Result).

run_echo_no_env_test() ->
    Result = ?M:run("echo -n $CPF_CMD"),
    ?assertEqual(ok, Result).

run_empty_test() ->
    ?assertException(error, badarg, ?M:run("")).
