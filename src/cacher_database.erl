%% @author Ryah Dahl <ry@tinycouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc Ensure that the relatively-installed dependencies are on the code
%%      loading path, and locate resources relative
%%      to this application's path.

-module(cacher_database).
-author('Ryah Dahl <ry@tinyclouds.org>').
-export([start/0, loop/2]).
-export([test/0]).
-include_lib("cacher.hrl").

start() ->
  register(cacher_database, spawn(cacher_database, loop, [[],[]])).

loop(RequestDatabase, IdDatabase) ->
  receive
    { store, Cache } ->
      Path = proplist_to_path(Cache#cache.request_filter),
      UpdatedRequestDatabase = path_tree:store(RequestDatabase, Path, Cache),
      %io:format("store: ~p~ndb: ~p~n", [Cache#cache.request_filter, UpdatedRequestDatabase]),
      loop(UpdatedRequestDatabase, IdDatabase);

    { expire, Id } ->
      io:format("expire: ~p~n", [Id]);
    { find, Request } ->
      io:format("find: ~p~n", [Request]);
    Unknown ->
      io:format("unknown message to database: ~p~n", [Unknown]),
      loop(RequestDatabase, IdDatabase)
  end.

% The RequestDatabase looks like this:
% [ {"Accept", 
%   [ { "text/html",
%

proplist_to_path([]) -> 
  [];
proplist_to_path([ { Key, Value } | Rest ]) -> 
  [ Key | proplist_to_path(Value) ++  proplist_to_path(Rest) ];
proplist_to_path(Node) -> [Node].

test_proplist_to_path() ->
  %io:format("proplist: ~p~n",[ proplist_to_path( [ { "Accept", "*/*" } ] ) ]),

  [ "Accept", "*/*" ] 
  = proplist_to_path( [ { "Accept", "*/*" } ] ),

  [ "Accept", "*/*", "Params", "Hello", "World" ] 
  = proplist_to_path( [ { "Accept", "*/*" } 
                      , { "Params", 
                          [ { "Hello", "World"}
                          ]
                        }
                      ] ),

  [ "Accept", "*/*", "Params", "Hello", "World", "Alpha", "Beta", "Cookies",
    "World", "Hello", "Alpha", "Beta" ] 
  = proplist_to_path( [ { "Accept", "*/*" } 
                      , { "Params", 
                          [ { "Hello", "World"}
                          , { "Alpha", "Beta"}
                          ]
                        }
                      , { "Cookies",
                          [ { "World", "Hello"}
                          , { "Alpha", "Beta"}
                          ]
                        }
                      ] ),
  ok.

test() ->
  ok = test_proplist_to_path(),
  ok.

