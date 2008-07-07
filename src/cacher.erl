%% @author Ryah Dahl <ry@tinycouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc TEMPLATE.

-module(cacher).
-author('Ryah Dahl <ry@tinyclouds.org>').
-export([start/0, stop/0]).
-export([test/0]).

ensure_started(App) ->
    case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
    end.
        
%% @spec start() -> ok
%% @doc Start the cacher server.
start() ->
    cacher_deps:ensure(),
    ensure_started(crypto),
%    debughelper:start(),
%    debughelper:trace(cacher_database, find),
%    debughelper:trace(cacher_database, maximal_path),
%    debughelper:trace(cacher_database, add_dir),
%    debughelper:trace(cacher_database, set_data),
%    debughelper:trace(cacher_database, get_data),
%    debughelper:trace(cacher_database, create_node),
%    debughelper:trace(cacher_database, store),
    application:start(cacher).

%% @spec stop() -> ok
%% @doc Stop the cacher server.
stop() ->
    Res = application:stop(cacher),
    application:stop(crypto),
    Res.

test() ->
    ok = path_tree:test(),
    ok = cacher_database:test(),
    ok = cacher_web:test(),
    io:format("Tests passed.~n"),
    ok.

