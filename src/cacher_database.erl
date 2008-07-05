%% @author Ryah Dahl <ry@tinycouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc Ensure that the relatively-installed dependencies are on the code
%%            loading path, and locate resources relative
%%            to this application's path.

-module(cacher_database).
-author('Ryah Dahl <ry@tinyclouds.org>').
-export([start/0, loop/1, serialize/1]).
-export([test/0]).
-include_lib("cacher.hrl").

start() ->
    RequestDatabase = [],
    ets:new(id_table, [public, named_table, bag]),
    ets:new(cache_table, [public, named_table]),
    Pid = spawn(cacher_database, loop, [RequestDatabase]),
    register(cacher_database, Pid).

loop(RequestDatabase) ->
    UpdatedRequestDatabase = receive
    { store, Cache } ->
        store(Cache, RequestDatabase);

    { expire, Identifiers, Client } ->
        
        expire(Identifiers),
        Client ! ok,
        RequestDatabase;

    { exists, Digest, Client } ->
        case ets:member(cache_table, Digest) of
        true ->
            Client ! yes;
        false ->
            Client ! no
        end,
        RequestDatabase;

    { find, RequestElements, Client } ->
        % io:format("find: ~p~n", [RequestElements]),
        Path = serialize(RequestElements),
        case path_tree:find(RequestDatabase, Path) of 
        not_found -> 
            Client ! not_found;
        {ok, Digest} -> 
            case ets:lookup_element(cache_table, Digest, 2) of
            badarg ->
                % path_tree:remove(ReqeustDatabase, Digest)
                Client ! not_found;
            Cache ->
                Client ! {ok, Cache}
            end
        end,
        RequestDatabase
    end,
    loop(UpdatedRequestDatabase). 

% depth first serialization
serialize([]) -> [];

serialize([ { Key, Value } | Rest ]) -> 
    [ Key | serialize(Value) ++ serialize(Rest) ];

serialize(Node) -> [Node].

expire(Identifiers) ->
    % io:format("expire: ~p~n", [Identifiers]),
    DeleteCache = fun (Digest) ->
        ets:delete(cache_table, Digest)
    end,

    ExpireId = fun (Id) ->
        case ets:lookup_element(id_table, Id, 2) of
        badarg ->
            ok;
        Digests ->
            lists:foreach(DeleteCache, Digests)
        end,
        ok
    end,
    lists:foreach(ExpireId, Identifiers),
    ok.

store(Cache, RequestDatabase) ->
    Path = serialize(Cache#cache.request_filter),
    Digest = Cache#cache.digest,

    ets:insert(cache_table, {Digest, Cache}), 

    UpdatedRequestDatabase = path_tree:store(RequestDatabase, Path, Digest),
    % io:format("requestdb: ~p~n~n", [UpdatedRequestDatabase]),

    InsertIdTable = fun (Id) ->
        ets:insert(id_table, {Id, Digest})
    end,
    lists:foreach(InsertIdTable, Cache#cache.identifiers),
    
    UpdatedRequestDatabase.


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

