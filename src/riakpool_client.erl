%% @author David Weldon
%% @doc riakpool_client is a collection of convenience functions for using
%% riakpool.

-module(riakpool_client).
-export([delete/2, get/2, list_keys/1, post/3, put/3]).

%% @doc Delete `Key' from `Bucket'.
-spec delete(binary(), binary()) -> ok.
delete(Bucket, Key) ->
    riakpool:execute(fun(C) -> riakc_pb_socket:delete(C, Bucket, Key) end), ok.

%% @doc Returns the value associated with `Key' in `Bucket' as `{ok, binary()}'.
%% If an error was encountered or the value was not present, returns
%% `{error, any()}'.
-spec get(binary(), binary()) -> {ok, binary()} | {error, any()}.
get(Bucket, Key) ->
    Fun =
        fun(C) ->
            case riakc_pb_socket:get(C, Bucket, Key) of
                {ok, O} -> riakc_obj:get_value(O);
                {error, E} -> {error, E}
            end
        end,
    case riakpool:execute(Fun) of
        {ok, Value} when is_binary(Value) -> {ok, Value};
        {ok, {error, E}} -> {error, E};
        {error, E} -> {error, E}
    end.

%% @doc Returns the list of keys in `Bucket' as `{ok, list()}'. If an error was
%% encountered, returns `{error, any()}'.
-spec list_keys(binary()) -> {ok, list()} | {error, any()}.
list_keys(Bucket) ->
    Fun = fun(C) -> riakc_pb_socket:list_keys(C, Bucket) end,
    case riakpool:execute(Fun) of
        {ok, {ok, Keys}} -> {ok, Keys};
        {error, E} -> {error, E}
    end.

%% @doc Associates `Key' with `Value' in `Bucket' without checking if `Key'
%% already exists in `Bucket'.
-spec post(binary(), binary(), binary()) -> ok.
post(Bucket, Key, Value) ->
    Fun =
        fun(C) ->
            Object = riakc_obj:new(Bucket, Key, Value),
            riakc_pb_socket:put(C, Object)
        end,
    riakpool:execute(Fun), ok.

%% @doc Associates `Key' with `Value' in `Bucket'. If `Key' already exists in
%% `Bucket', an update will be preformed.
-spec put(binary(), binary(), binary()) -> ok.
put(Bucket, Key, Value) ->
    Fun =
        fun(C) ->
            case riakc_pb_socket:get(C, Bucket, Key) of
                {ok, O} ->
                    O2 = riakc_obj:update_value(O, Value),
                    riakc_pb_socket:put(C, O2);
                {error, _} ->
                    O = riakc_obj:new(Bucket, Key, Value),
                    riakc_pb_socket:put(C, O)
            end
        end,
    riakpool:execute(Fun), ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

client_test() ->
    {B, K, V1, V2} = {<<"groceries">>, <<"mine">>, <<"eggs">>, <<"toast">>},

    %% start the application
    application:start(riakpool),

    %% nothing should work until the pool is started
    ?assertMatch({error, _}, list_keys(B)),
    ?assertMatch({error, _}, get(B, K)),

    %% start the pool
    riakpool:start_pool(),

    %% verify nothing is in B
    ?assertEqual({ok, []}, list_keys(B)),
    ?assertMatch({error, notfound}, get(B, K)),

    %% post {K, V1}
    ?assertEqual(ok, post(B, K, V1)),
    ?assertEqual({ok, V1}, get(B, K)),

    %% delete K
    ?assertEqual(ok, delete(B, K)),
    ?assertMatch({error, notfound}, get(B, K)),

    %% put {K, V1}
    ?assertEqual(ok, put(B, K, V1)),
    ?assertEqual({ok, V1}, get(B, K)),

    %% put {K, V2}
    ?assertEqual(ok, put(B, K, V2)),
    ?assertEqual({ok, V2}, get(B, K)),

    %% verify K is in the list of keys
    ?assertEqual({ok, [K]}, list_keys(B)),

    %% delete K
    ?assertEqual(ok, delete(B, K)),
    ?assertMatch({error, notfound}, get(B, K)),

    %% stop the application
    application:stop(riakpool).

-endif.
