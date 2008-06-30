%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(cacher).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the cacher server.
start() ->
    cacher_deps:ensure(),
    ensure_started(crypto),
    application:start(cacher).

%% @spec stop() -> ok
%% @doc Stop the cacher server.
stop() ->
    Res = application:stop(cacher),
    application:stop(crypto),
    Res.
