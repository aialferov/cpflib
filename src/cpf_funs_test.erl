-module(cpf_funs_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, cpf_funs).

do_while_plain_test() ->
    ?assertEqual(6.0, ?M:do_while([
        {sum, fun lists:sum/1, [[1,2,3,4,5]]},
        {double, fun(X) -> 2*X end, [{sum}]},
        {divide, fun(X, Div) -> X/Div end, [{double}, 5]}
    ])).

do_while_ok_intermediate_test() ->
    ?assertEqual(6.0, ?M:do_while([
        {sum, fun lists:sum/1, [[1,2,3,4,5]]},
        {double, fun(X) -> {ok, 2*X} end, [{sum}]},
        {divide, fun(X, Div) -> X/Div end, [{double}, 5]}
    ])).

do_while_ok_last_test() ->
    ?assertEqual({ok, 6.0}, ?M:do_while([
        {sum, fun lists:sum/1, [[1,2,3,4,5]]},
        {double, fun(X) -> {ok, 2*X} end, [{sum}]},
        {divide, fun(X, Div) -> {ok, X/Div} end, [{double}, 5]}
    ])).

do_while_error_test() ->
    ?assertEqual(error, ?M:do_while([
        {sum, fun lists:sum/1, [[1,2,3,4,5]]},
        {double, fun(_X) -> error end, [{sum}]},
        {divide, fun(X, Div) -> {ok, X/Div} end, [{double}, 5]}
    ])).

do_while_error_reason_test() ->
    ?assertEqual({error, not_implemented}, ?M:do_while([
        {sum, fun lists:sum/1, [[1,2,3,4,5]]},
        {double, fun(_X) -> {error, not_implemented} end, [{sum}]},
        {divide, fun(X, Div) -> {ok, X/Div} end, [{double}, 5]}
    ])).

do_while_false_test() ->
    ?assertEqual(false, ?M:do_while([
        {sum, fun lists:sum/1, [[1,2,3,4,5]]},
        {double, fun(_X) -> false end, [{sum}]},
        {divide, fun(X, Div) -> {ok, X/Div} end, [{double}, 5]}
    ])).

do_while_wrong_id_test() ->
    ?assertException(error, {case_clause, false}, ?M:do_while([
        {sum, fun lists:sum/1, [[1,2,3,4,5]]},
        {double, fun(_X) -> false end, [{wrong_sum}]},
        {divide, fun(X, Div) -> {ok, X/Div} end, [{double}, 5]}
    ])).

do_while_wrong_not_used_id_test() ->
    ?assertEqual({error, not_implemented}, ?M:do_while([
        {sum, fun lists:sum/1, [[1,2,3,4,5]]},
        {double, fun(_X) -> {error, not_implemented} end, [{sum}]},
        {divide, fun(X, Div) -> {ok, X/Div} end, [{wrong_double}, 5]}
    ])).

do_while_empty_fun_list_test() ->
    ?assertException(error, badarg, ?M:do_while([])).
