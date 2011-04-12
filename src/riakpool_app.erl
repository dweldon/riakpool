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
    application:start(riakpool),
    riakpool:start_pool(),
    Fun = fun(C) -> riakc_pb_socket:ping(C) end,
    ?assertEqual({ok, pong}, riakpool:execute(Fun)),
    application:stop(riakpool).

-endif.
