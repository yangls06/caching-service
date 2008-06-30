%% @author Ryah Dahl <ry@tinycouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc Ensure that the relatively-installed dependencies are on the code
%%      loading path, and locate resources relative
%%      to this application's path.

-module(cacher_database).
-author('Ryah Dahl <ry@tinyclouds.org>').

-export([start/0, loop/0]).

loop() ->
  receive
    { store, RequestFilter, Ids, ContentType, Data } ->
      io:format("store: ~p~n", [RequestFilter]);
    { expire, Id } ->
      io:format("expire: ~p~n", [Id]);
    { find, Request } ->
      io:format("find: ~p~n", [Request]);
    Unknown ->
      io:format("unknown message to database: ~p~n", [Unknown]) 
  end,
  loop().


start() ->
  register(cacher_database, spawn(cacher_database, loop, [])).

