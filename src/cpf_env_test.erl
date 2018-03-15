-module(cpf_env_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, cpf_env).

read_binary_ok_test() ->
    ?assertEqual({ok, [
            {app1, [{"k11", "v11"}, {"k12", "v12"}]},
            {app2, [{"k21", "v21"}, {"k22", "v22"}]}
        ]},
        ?M:read_binary(<<"[
            {app1, [{\"k11\", \"v11\"}, {\"k12\", \"v12\"}]},
            {app2, [{\"k21\", \"v21\"}, {\"k22\", \"v22\"}]}
        ].">>)
    ).

read_binary_error_no_period_test() ->
    ?assertEqual(
        {error,{4,erl_parse,["syntax error before: ",[]]}},
        ?M:read_binary(<<"[
            {app1, [{\"k11\", \"v11\"}, {\"k12\", \"v12\"}]},
            {app2, [{\"k21\", \"v21\"}, {\"k22\", \"v22\"}]}
        ]">>)
    ).

read_binary_error_test() ->
    ?assertEqual(
        {error,{2,erl_parse,["syntax error before: ","','"]}},
        ?M:read_binary(<<"[
            {, [{\"k11\", \"v11\"}, {\"k12\", \"v12\"}]},
            {app2, [{\"k21\", \"v21\"}, {\"k22\", \"v22\"}]}
        ].">>)
    ).

read_binary_empty_list_test() ->
    ?assertEqual({ok, []}, ?M:read_binary(<<"[].">>)).

read_binary_empty_test() ->
    ?assertEqual(
        {error,{0,erl_parse,["syntax error before: ",[]]}},
        ?M:read_binary(<<"">>)
    ).
