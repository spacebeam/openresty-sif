-module(files_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    application:ensure_all_started(econfig),
    econfig:register_config(engine, ["/etc/files.conf"], [autoreload]),
    econfig:subscribe(engine),
    Port = econfig:get_integer(engine, "engine", "port"),
    % missing riak server configuration file option.
    Riak = econfig:get_value(engine, "engine", "riak"),
    % init test with  the erlang riak client
    {ok, Pid} = riakc_pb_socket:start_link(Riak, 8087),
    % dispatch cowboy router
    Dispatch = cowboy_router:compile([
        {'_', [
                % hello, hello!
                {"/chunks/", chunk_handler,[]},
                % missing :chunk_uuid implementation!
                {"/chunks/:chunk_uuid", chunk_handler,[]}
            ]}
    ]),
    {ok,_} = cowboy:start_clear(http,
        [{port, Port}],
      #{env => #{dispatch => Dispatch}}
    ),
    sloth_sup:start_link().

stop(_State) ->
    ok.
