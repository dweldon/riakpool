%% @author David Weldon
%% @doc riakpool implements a pool of riak protocol buffer clients. In order to
%% use a connection, a call to {@link execute/1} must be made. This will check
%% out a connection from the pool, use it, and check it back in. This ensures
%% that a given connection can only be in use by one external process at a time.
%% If no existing connections are found, a new one will be established. Note
%% this means the pool will always be the size of the last peak need. The number
%% of connections can be checked with {@link count/0}.
%%
%% Prior to any calls to {@link execute/1}, the pool must be started. This can
%% be accomplished in one of two ways:
%%
%% 1. Before the server is started, set the riakpool application environment
%%    variables `riakpool_host' and `riakpool_port'.
%%
%% 2. After the server is started, call {@link start_pool/0} or
%%    {@link start_pool/2}

-module(riakpool).
-behaviour(gen_server).
-export([count/0,
         execute/1,
         start_link/0,
         start_pool/0,
         start_pool/2,
         stop/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-record(state, {host, port, pids}).

%% @type host() = string() | atom().

%% @spec count() -> integer()
%% @doc Returns the number of connections as seen by the supervisor.
count() ->
    Props = supervisor:count_children(riakpool_connection_sup),
    case proplists:get_value(active, Props) of
        N when is_integer(N) -> N;
        undefined -> 0
    end.

%% @spec execute(Fun) -> {ok, Value::any()} | {error, any()}
%%       Fun = function(pid())
%% @doc Finds the next available connection pid from the pool and calls
%% `Fun(Pid)'. Returns `{ok, Value}' if the call was successful, and
%% `{error, any()}' otherwise. If no connection could be found, a new connection
%% will be established.
%% ```
%% > riakpool:execute(fun(C) -> riakc_pb_socket:ping(C) end).
%% {ok,pong}
%% '''
execute(Fun) ->
    case gen_server:call(?MODULE, check_out) of
        {ok, Pid} ->
            try {ok, Fun(Pid)}
            catch _:E -> {error, E}
            after gen_server:cast(?MODULE, {check_in, Pid}) end;
        {error, E} -> {error, E}
    end.

%% @spec start_link() -> {ok, pid()} | {error, any()}
%% @doc Starts the server.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec start_pool() -> ok | {error, any()}
%% @doc Starts a connection pool to a server listening on {"127.0.0.1", 8087}.
%% Note that a pool can only be started once.
start_pool() -> start_pool("127.0.0.1", 8087).

%% @spec start_pool(host(), integer()) -> ok | {error, any()}
%% @doc Starts a connection pool to a server listening on {`Host', `Port'}.
%% Note that a pool can only be started once.
start_pool(Host, Port) when is_integer(Port) ->
    gen_server:call(?MODULE, {start_pool, Host, Port}).

%% @spec stop() -> ok
%% @doc Stops the server.
stop() -> gen_server:cast(?MODULE, stop).

%% @hidden
init([]) ->
    process_flag(trap_exit, true),
    case [application:get_env(P) || P <- [riakpool_host, riakpool_port]] of
        [{ok, Host}, {ok, Port}] when is_integer(Port) ->
            {ok, new_state(Host, Port)};
        _ -> {ok, undefined}
    end.

%% @hidden
handle_call({start_pool, Host, Port}, _From, undefined) ->
    case new_state(Host, Port) of
        State=#state{} -> {reply, ok, State};
        undefined -> {reply, {error, connection_error}, undefined}
    end;
handle_call({start_pool, _Host, _Port}, _From, State=#state{}) ->
    {reply, {error, pool_already_started}, State};
handle_call(check_out, _From, undefined) ->
    {reply, {error, pool_not_started}, undefined};
handle_call(check_out, _From, State=#state{host=Host, port=Port, pids=Pids}) ->
    case next_pid(Host, Port, Pids) of
        {ok, Pid, NewPids} -> {reply, {ok, Pid}, State#state{pids=NewPids}};
        {error, NewPids} ->
            {reply, {error, connection_error}, State#state{pids=NewPids}}
    end;
handle_call(_Request, _From, State) -> {reply, ok, State}.

%% @hidden
handle_cast({check_in, Pid}, State=#state{pids=Pids}) ->
    NewPids = queue:in(Pid, Pids),
    {noreply, State#state{pids=NewPids}};
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.

%% @hidden
handle_info(_Info, State) -> {noreply, State}.

%% @hidden
terminate(_Reason, undefined) -> ok;
terminate(_Reason, #state{pids=Pids}) ->
    StopFun =
        fun(Pid) ->
            case is_process_alive(Pid) of
                true -> riakc_pb_socket:stop(Pid);
                false -> ok
            end
        end,
    [StopFun(Pid) || Pid <- queue:to_list(Pids)], ok.

%% @hidden
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @spec new_state(host(), integer()) -> state() | undefined
%% @doc Returns a state with a single pid if a connection could be established,
%% otherwise returns undefined.
new_state(Host, Port) ->
    case new_connection(Host, Port) of
        {ok, Pid} ->
            #state{host=Host, port=Port, pids=queue:in(Pid, queue:new())};
        error -> undefined
    end.

%% @spec new_connection(host(), integer()) -> {ok, Pid} | error
%% @doc Returns {ok, Pid} if a new connection was established and added to the
%% supervisor, otherwise returns error.
new_connection(Host, Port) ->
    case supervisor:start_child(riakpool_connection_sup, [Host, Port]) of
        {ok, Pid} when is_pid(Pid) -> {ok, Pid};
        {ok, Pid, _} when is_pid(Pid) -> {ok, Pid};
        _ -> error
    end.

%% @spec next_pid(host(), integer(), queue()) -> {ok, pid(), queue()} |
%%                                               {error, queue()}
%% @doc Recursively dequeues Pids in search of a live connection. Dead
%% connections are removed from the queue as it is searched. If no connection
%% pid could be found, a new one will be established. Returns {ok, Pid, NewPids}
%% where NewPids is the queue after any necessary dequeues. Returns error if no
%% live connection could be found and no new connection could be established.
next_pid(Host, Port, Pids) ->
    case queue:out(Pids) of
        {{value, Pid}, NewPids} ->
            case is_process_alive(Pid) of
                true -> {ok, Pid, NewPids};
                false -> next_pid(Host, Port, NewPids)
            end;
        {empty, _} ->
            case new_connection(Host, Port) of
                {ok, Pid} -> {ok, Pid, Pids};
                error -> {error, Pids}
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

execute_test() ->
    riakpool_connection_sup:start_link(),
    riakpool:start_link(),
    riakpool:start_pool(),
    ?assertEqual(1, count()),
    Fun1 = fun(C) -> riakc_pb_socket:ping(C) end,
    Fun2 = fun(_) -> riakc_pb_socket:ping(1) end,
    ?assertEqual({ok, pong}, execute(Fun1)),
    ?assertMatch({error, _}, execute(Fun2)),
    ?assertEqual({ok, pong}, execute(Fun1)),
    riakpool:stop(),
    timer:sleep(10),
    ?assertEqual(0, count()).

execute_error_test() ->
    riakpool:start_link(),
    Fun = fun(C) -> riakc_pb_socket:ping(C) end,
    ?assertEqual({error, pool_not_started}, execute(Fun)),
    riakpool:stop(),
    timer:sleep(10),
    ?assertEqual(0, count()).

start_pool_test() ->
    riakpool_connection_sup:start_link(),
    riakpool:start_link(),
    {H, P} = {"localhost", 8000},
    ?assertEqual({error, connection_error}, riakpool:start_pool(H, P)),
    ?assertEqual(ok, riakpool:start_pool()),
    ?assertEqual({error, pool_already_started}, riakpool:start_pool()),
    riakpool:stop(),
    timer:sleep(10),
    ?assertEqual(0, count()).

next_pid_test() ->
    riakpool_connection_sup:start_link(),
    {H, P} = {"localhost", 8087},
    ?assertEqual(0, count()),
    {ok, P1} = new_connection(H, P),
    {ok, P2} = new_connection(H, P),
    {ok, P3} = new_connection(H, P),
    ?assertEqual(3, count()),
    riakc_pb_socket:stop(P1),
    riakc_pb_socket:stop(P2),
    ?assertEqual(1, count()),
    Q0 = queue:new(),
    Q = queue:from_list([P1, P2, P3]),
    ?assertMatch({ok, P3, Q0}, next_pid(H, P, Q)),
    riakc_pb_socket:stop(P3),
    {ok, P4, Q0} = next_pid(H, P, Q0),
    ?assertEqual(1, count()),
    riakc_pb_socket:stop(P4),
    ?assertEqual(0, count()).

next_pid_error_test() ->
    {H, P} = {"localhost", 8000},
    Q0 = queue:new(),
    ?assertMatch({error, Q0}, next_pid(H, P, Q0)).

-endif.
