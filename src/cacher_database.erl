%% @author Ryah Dahl <ry@tinycouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc Ensure that the relatively-installed dependencies are on the code
%%            loading path, and locate resources relative
%%            to this application's path.

-module(cacher_database).
-author('Ryah Dahl <ry@tinyclouds.org>').
-export([start/0, loop/0, serialize/1]).
-export([test/0]).
-include_lib("cacher.hrl").

start() ->
    ets:new(path_tree,   [public, named_table, ordered_set]),
    ets:new(cache_table, [public, named_table, ordered_set]),
    ets:new(id_table,    [public, named_table, bag]),
    register(cacher_database, spawn(cacher_database, loop, [])).

loop() ->
    receive
    { store, Cache } ->
        Path = serialize(Cache#cache.request_filter),
        Digest = Cache#cache.digest,
        ets:insert(cache_table, {Digest, Cache}), 

        case path_tree:store(Path, Digest) of
        ok ->
            ok;
        {replaced, Digest} ->
            ets:delete(cache_table, Digest)
        end,

        InsertIdTable = fun (Id) ->
            ets:insert(id_table, {Id, Digest})
        end,
        lists:foreach(InsertIdTable, Cache#cache.identifiers);

    { expire, Identifiers, Client } ->
        
        expire(Identifiers),
        Client ! ok;

    { exists, Digest, Client } ->
        case ets:member(cache_table, Digest) of
        true ->
            Client ! yes;
        false ->
            Client ! no
        end;

    { find, RequestElements, Client } ->
        % io:format("find: ~p~n", [RequestElements]),
        Path = serialize(RequestElements),
        case path_tree:find(Path) of 
        not_found -> 
            Client ! not_found;
        {ok, Digest} -> 
            case ets:lookup(cache_table, Digest) of
            [] ->
                Client ! not_found;
            [{_, Cache}] ->
                Client ! {ok, Cache}
            end
        end
    end,
    loop(). 

% depth first serialization
serialize([]) -> [];

serialize([ { Key, Value } | Rest ]) -> 
    [ Key | serialize(Value) ++ serialize(Rest) ];

serialize(Node) -> [Node].

expire(Identifiers) ->
    % io:format("expire: ~p~n", [Identifiers]),
    DeleteCache = fun ({_, Digest}) ->
        ets:delete(cache_table, Digest)
    end,

    ExpireId = fun (Id) ->
        case ets:lookup(id_table, Id) of
        [] ->
            ok;
        Digests ->
            lists:foreach(DeleteCache, Digests)
        end,
        ok
    end,
    lists:foreach(ExpireId, Identifiers),
    ok.


test_serialize() ->
    %io:format("proplist: ~p~n",[ serialize( [ { "Accept", "*/*" } ] ) ]),

    [ "Accept", "*/*" ] 
    = serialize( [ { "Accept", "*/*" } ] ),

    [ "Accept", "*/*", "Params", "Hello", "World" ] 
    = serialize( [ { "Accept", "*/*" } 
                   , { "Params", 
                       [ { "Hello", "World"}
                       ]
                     }
                 ] ),

    [ "Accept", "*/*", "Params", "Hello", "World", "Alpha", "Beta", "Cookies",
        "World", "Hello", "Alpha", "Beta" ] 
    = serialize( [ { "Accept", "*/*" } 
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
    ok = test_serialize(),
    ok.

