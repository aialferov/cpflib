-module(cpf_env).

-export([
    load/1,

    read_files/1,
    read_file/1,
    read_binary/1,

    set/1
]).

load([]) -> read_files([]);

load(Paths = [H|_]) when is_list(H) ->
    case read_files(Paths) of
        {ok, {{Path, Env}, _Errors}} -> {set(Env), Path};
        {error, Errors} -> {error, Errors}
    end;

load(Path = [H|_]) when is_number(H) ->
    case read_file(Path) of
        {ok, Env} -> set(Env);
        {error, Reason} -> {error, Reason}
    end;

load(Binary) when is_binary(Binary) ->
    case read_binary(Binary) of
        {ok, Env} -> set(Env);
        {error, Reason} -> {error, Reason}
    end.

read_files([]) -> {error, []};
read_files(Paths) ->
    Results = lists:foldl(fun read_file/2, #{}, Paths),
    Errors = lists:reverse(maps:get(errors, Results, [])),
    case maps:is_key(ok, Results) of
        true -> {ok, {maps:get(ok, Results), Errors}};
        false -> {error, Errors}
    end.

read_file(_Path, Results = #{ok := _Result}) -> Results;
read_file(Path, Results) ->
    case read_file(Path) of
        {ok, Env} -> maps:put(ok, {Path, Env}, Results);
        {error, Reason} ->
            Errors = maps:get(errors, Results, []),
            maps:put(errors, [{Path, Reason}|Errors], Results)
    end.

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Binary} -> read_binary(Binary);
        {error, Reason} -> {error, Reason}
    end.

read_binary(Binary) ->
    cpf_funs:apply_while([
        {content, fun binary_to_list/1, [Binary]},
        {tokens, fun erl_scan_string/1, [{content}]},
        {env, fun erl_parse:parse_term/1, [{tokens}]}
    ]).

erl_scan_string(Content) ->
    case erl_scan:string(Content) of
        {ok, Tokens, _EndLocation} -> {ok, Tokens};
        {error, ErrorInfo, ErrorLocation} -> {error, {ErrorInfo, ErrorLocation}}
    end.

set(Env) -> lists:foreach(fun set_app_env/1, Env).
set_app_env({App, Env}) -> lists:foreach(set_app_env_fun(App), Env).
set_app_env_fun(App) -> fun({Par, Val}) ->
    application:set_env(App, Par, Val, [{persistent, true}])
end.
