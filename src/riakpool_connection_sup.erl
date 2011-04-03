%% @author David Weldon
%% @hidden

-module(riakpool_connection_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(ADDRESS, "127.0.0.1").
-define(PORT, 8087).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Ip = case application:get_env(riak_pb_ip) of
  {ok,Ip0} -> Ip0;
  _ -> ?ADDRESS
    end,

    Port = case application:get_env(riak_pb_port) of
  {ok,Port0} when is_integer(Port0) -> Port0;
  _ -> ?PORT
    end,

    {ok, {{simple_one_for_one, 0, 1},
          [{connections, {riakc_pb_socket, start_link, [Ip, Port]},
            temporary, brutal_kill, worker, [riakc_pb_socket]}]}}.
