%% @author David Weldon
%% @hidden

-module(riakpool_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    riakpool_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

app_test() ->
    application:set_env(riakpool, riakpool_host, "localhost"),
    application:set_env(riakpool, riakpool_port, 8087),
    application:start(riakpool),
    Fun = fun(C) -> riakc_pb_socket:ping(C) end,
    ?assertEqual({ok, pong}, riakpool:execute(Fun)),
    ?assertEqual(1, riakpool:count()),
    application:stop(riakpool).

-endif.
