-module(cpf_node).

-export([
    start/1, start/2, start/3,
    stop/0,

    ensure_epmd/0, ensure_epmd/3,

    epmd_start/0,
    epmd_stop/0,

    epmd_wait/0, epmd_wait/3
]).

-define(Cmd, cpf_cmd).
-define(Funs, cpf_funs).

-define(EpmdStartCmd, "epmd -daemon").
-define(EpmdStopCmd, "epmd -kill").

-define(EpmdHost, "localhost").
-define(EpmdPort, 4369).
-define(EpmdReadiness, {50, 6}). % first delay, retries; ~6 seconds

-define(EpmdArgs, [?EpmdHost, ?EpmdPort, ?EpmdReadiness]).

start(ShortName) -> start(ShortName, ?EpmdArgs).

start(ShortName, Cookie) when is_atom(Cookie) ->
    start(ShortName, Cookie, ?EpmdArgs);

start(ShortName, EpmdArgs) ->
    Name = list_to_atom(atom_to_list(ShortName) ++ "@localhost"),
    start(Name, ShortName, EpmdArgs).

start(Name, Cookie, EpmdArgs) -> ?Funs:apply_while([
    {epmd, fun start_ensure_epmd/3, EpmdArgs},
    {kernel, fun net_kernel:start/1, [[Name, shortnames]]},
    {cookie, fun erlang:set_cookie/2, [Name, Cookie]},
    {ok, fun() -> ok end, []}
]).

stop() ->
    net_kernel:stop().

start_ensure_epmd(Host, Port, Readiness) ->
    case ensure_epmd(Host, Port, Readiness) of
        ok -> ok;
        {error, Reason} -> {error, {epmd, Reason}}
    end.

ensure_epmd() -> erlang:apply(fun ensure_epmd/3, ?EpmdArgs).
ensure_epmd(Host, Port, Readiness) ->
    case epmd_start() of
        ok -> epmd_wait(Host, Port, Readiness);
        {error, Reason} -> {error, Reason}
    end.

epmd_start() -> ?Cmd:run(?EpmdStartCmd).

epmd_stop() ->
    case ?Cmd:run(?EpmdStopCmd) of
        {ok, _Data} -> ok;
        {error, Reason} -> {error, Reason}
    end.

epmd_wait() -> erlang:apply(fun epmd_wait/3, ?EpmdArgs).
epmd_wait(Host, Port, {Timeout, Retries}) ->
    case gen_tcp:connect(Host, Port, []) of
        {ok, Socket} ->
            gen_tcp:close(Socket);
        {error, _Reason} ->
            timer:sleep(Timeout),
            if Retries > 0 -> epmd_wait(Host, Port, {2*Timeout, Retries-1});
               Retries < 1 -> {error, timeout} end
    end.
