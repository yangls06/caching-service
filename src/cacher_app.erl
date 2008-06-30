%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the cacher application.

-module(cacher_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for cacher.
start(_Type, _StartArgs) ->
    cacher_deps:ensure(),
    cacher_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for cacher.
stop(_State) ->
    ok.
