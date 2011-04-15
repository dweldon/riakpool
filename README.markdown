<p align="right">
    <img src="http://www.basho.com/images/riaklogo.png">
</p>

Overview
--------
riakpool is an application for maintaining a dynamic pool of protocol buffer
client connections to a riak database. It ensures that a given connection can
only be in use by one external process at a time.

Installation
------------
    $ git clone git@github.com:iac/riakpool.git
    $ cd riakpool
    $ make

Interface
---------
The following example gives an overview of the riakpool interface. Please see
the complete documentation by running `make doc`.

    1> application:start(riakpool).
    ok
    2> riakpool:start_pool("127.0.0.1", 8087).
    ok
    3> riakpool:execute(fun(C) -> riakc_pb_socket:ping(C) end).
    {ok,pong}
    4> riakpool_client:put(<<"groceries">>, <<"mine">>, <<"eggs">>).
    ok
    5> riakpool_client:get(<<"groceries">>, <<"mine">>).
    {ok,<<"eggs">>}
    6> riakpool_client:list_keys(<<"groceries">>).
    {ok,[<<"mine">>]}
    7> riakpool_client:delete(<<"groceries">>, <<"mine">>).
    ok
    8> riakpool:count().
    1

Note that the use of riakpool_client is completely optional - it is simply a
collection of convenience functions which call riakpool:execute/1.

Starting the Pool
-----------------
Prior to any calls to `riakpool:execute/1`, the pool must be started. This can
be accomplished in one of two ways:

1. Before the server is started, set the riakpool application environment
   variables `riakpool_host` and `riakpool_port`.
2. After the server is started, call `riakpool:start_pool/0` or
   `riakpool:start_pool/2` (see previous section).
