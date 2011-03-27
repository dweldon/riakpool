%% @author David Weldon
%% @doc riakpool implements a simple pool of riak protocol buffer clients. When
%% the server is started, a single client connection is established. In order to
%% use a connection, a call to {@link execute/1} must be made. This will check
%% out a connection from the pool, use it, and check it back in. This ensures
%% that a given connection can only be in use by one external process at a time.
%% If no existing connections are found, a new one will be established. Note
%% this means that the pool will always be the size of the last peak need. The
%% number of connections can be checked with {@link count/0}.
%%
%% Currently, riakpool will only connect to port 8087 on 127.0.0.1.

-module(riakpool).
-behaviour(gen_server).
-export([count/0,
         execute/1,
         start_link/0,
         stop/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% @spec count() -> integer()
%% @doc Returns the number of connections as seen by the supervisor.
count() ->
    Props = supervisor:count_children(riakpool_connection_sup),
    case proplists:get_value(active, Props) of
        N when is_integer(N) -> N;
        undefined -> 0
    end.

%% @spec execute(Fun) -> {ok, Value::any()} | error
%%       Fun = function(pid())
%% @doc Finds the next available connection pid from the pool and calls
%% `Fun(Pid)'. Returns `{ok, Value}' if the call was successful, and `error'
%% otherwise. If no connection could be found, a new connection will be
%% established.
execute(Fun) ->
    case gen_server:call(?MODULE, check_out) of
        {ok, Pid} ->
            try {ok, Fun(Pid)}
            catch _:_ -> error
            after gen_server:cast(?MODULE, {check_in, Pid}) end;
        error -> error
    end.

%% @spec start_link() -> {ok, pid()} | {error, any()}
%% @doc Starts the server.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Stops the server.
stop() -> gen_server:cast(?MODULE, stop).

%% @hidden
init([]) ->
    {ok, Pid} = new_connection(),
    Pids = queue:in(Pid, queue:new()),
    {ok, Pids}.

%% @hidden
handle_call(check_out, _From, Pids) ->
    case next_pid(Pids) of
        {ok, Pid, NewPids} -> {reply, {ok, Pid}, NewPids};
        {error, NewPids} -> {reply, error, NewPids}
    end;
handle_call(_Request, _From, State) -> {reply, ok, State}.

%% @hidden
handle_cast({check_in, Pid}, Pids) -> {noreply, queue:in(Pid, Pids)};
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.

%% @hidden
handle_info(_Info, State) -> {noreply, State}.

%% @hidden
terminate(_Reason, Pids) ->
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

%% @spec new_connection() -> {ok, Pid} | error
%% @doc Returns {ok, Pid} if a new connection was established and added to the
%% supervisor, otherwise returns error.
new_connection() ->
    case supervisor:start_child(riakpool_connection_sup, []) of
        {ok, Pid} when is_pid(Pid) -> {ok, Pid};
        {ok, Pid, _} when is_pid(Pid) -> {ok, Pid};
        _ -> error
    end.

%% @spec next_pid(queue()) -> {ok, pid(), queue()} | {error, queue()}
%% @doc Recursively dequeues Pids in search of a live connection. Dead
%% connections are removed from the queue as it is searched. If no connection
%% pid could be found, a new one will be established. Returns {ok, Pid, NewPids}
%% where NewPids is the queue after any necessary dequeues. Returns error if no
%% live connection could be found and no new connection could be established.
next_pid(Pids) ->
    case queue:out(Pids) of
        {{value, Pid}, NewPids} ->
            case is_process_alive(Pid) of
                true -> {ok, Pid, NewPids};
                false -> next_pid(NewPids)
            end;
        {empty, _} ->
            case new_connection() of
                {ok, Pid} -> {ok, Pid, Pids};
                error -> {error, Pids}
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

    execute_test() ->
        riakpool_connection_sup:start_link(),
        riakpool:start_link(),
        ?assertEqual(1, count()),
        Fun1 = fun(C) -> riakc_pb_socket:ping(C) end,
        Fun2 = fun(_) -> riakc_pb_socket:ping(1) end,
        ?assertEqual({ok, pong}, execute(Fun1)),
        ?assertEqual(error, execute(Fun2)),
        ?assertEqual({ok, pong}, execute(Fun1)),
        riakpool:stop(),
        timer:sleep(10),
        ?assertEqual(0, count()).

    next_pid_test() ->
        riakpool_connection_sup:start_link(),
        ?assertEqual(0, count()),
        {ok, P1} = new_connection(),
        {ok, P2} = new_connection(),
        {ok, P3} = new_connection(),
        ?assertEqual(3, count()),
        riakc_pb_socket:stop(P1),
        riakc_pb_socket:stop(P2),
        ?assertEqual(1, count()),
        Q0 = queue:new(),
        Q = queue:from_list([P1, P2, P3]),
        ?assertMatch({ok, P3, Q0}, next_pid(Q)),
        riakc_pb_socket:stop(P3),
        {ok, P4, Q0} = next_pid(Q0),
        ?assertEqual(1, count()),
        riakc_pb_socket:stop(P4),
        ?assertEqual(0, count()).

-endif.
