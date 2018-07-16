-module(cpf_cmd).

-export([
    run/1, run/2
]).

-type cmd() :: string() |
               {string(), args()}. % template for "io:format/2"

-type args() :: [term()]. % arguments for template for "io:format/2"

-type option() :: {cd, Dir :: string()} |
                  {env, Env :: [{Name :: string(), Value :: string()}]}.

-type result() :: ok |
                  {ok, Data :: string()} |
                  {error, Data :: string()}.

-spec run(Cmd :: cmd()) -> result().
run(Cmd) -> run(Cmd, []).

-spec run(Cmd :: cmd(), Options :: [option()]) -> result().
run({Template, Args}, Opts) ->
    run(lists:flatten(io_lib:format(Template, Args)), Opts);

run(Cmd, Opts) ->
    PortName = {spawn, Cmd},
    PortSettings = [exit_status, stderr_to_stdout|Opts],
    Port = erlang:open_port(PortName, PortSettings),

    case wait(Port, "") of
        {ok, ""} -> ok;
        Result -> Result
    end.

wait(Port, TotalData) ->
    receive
        {Port, {data, Data}} -> wait(Port, TotalData ++ Data);
        {Port, {exit_status, 0}} -> {ok, TotalData};
        {Port, {exit_status, Status}} -> {error, {Status, TotalData}}
    end.
