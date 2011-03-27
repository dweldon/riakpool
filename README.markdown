<p align="right">
    <img src="http://www.basho.com/images/riaklogo.png">
</p>

Overview
--------
riakpool is an application for maintaining a dynamic pool of protocol buffer
client connections to a riak database. It ensures that a given connection can
only be in use by one external process at a time. Currently, riakpool will only
connect to port 8087 on 127.0.0.1.

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
    2> riakpool:execute(fun(C) -> riakc_pb_socket:ping(C) end).
    {ok,pong}
    3> riakpool:count().
    1
