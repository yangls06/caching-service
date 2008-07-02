%% @author Ryah Dahl <ry@tinycouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc Ensure that the relatively-installed dependencies are on the code
%%            loading path, and locate resources relative
%%            to this application's path.

-module(cacher_database).
-author('Ryah Dahl <ry@tinyclouds.org>').
-export([start/0, loop/3, proplist_to_path/1]).
-export([test/0]).
-include_lib("cacher.hrl").

start() ->
    RequestDatabase = [],
    IdDatabase = dict:new(),
    CacheDatabase = dict:new(),
    Pid = spawn(cacher_database, loop, [RequestDatabase, IdDatabase, CacheDatabase]),
    register(cacher_database, Pid).

loop(RequestDatabase, IdDatabase, CacheDatabase) ->
    {UpdatedRequestDatabase, UpdatedIdDatabase, UpdatedCacheDatabase} = receive
    { store, Cache } ->
        store(Cache, RequestDatabase, IdDatabase, CacheDatabase);

    { expire, Identifiers, Client } ->
        
        {UIdDatabase, UCacheDatabase} 
            = expire(Identifiers, IdDatabase, CacheDatabase),
        Client ! ok,
        { RequestDatabase, UIdDatabase, UCacheDatabase };

    { find, RequestElements, Client } ->
        % io:format("find: ~p~n", [RequestElements]),
        Path = proplist_to_path(RequestElements),
        case path_tree:find(RequestDatabase, Path) of 
        not_found -> 
            Client ! not_found;
        {ok, Digest} -> 
            case dict:find(Digest, CacheDatabase) of
            {ok, Cache} ->
                Client ! {ok, Cache};
            error ->
                % path_tree:remove(ReqeustDatabase, Digest)
                Client ! not_found
            end
        end,
        { RequestDatabase, IdDatabase, CacheDatabase }
    end,
    loop(UpdatedRequestDatabase, UpdatedIdDatabase, UpdatedCacheDatabase). 

proplist_to_path([]) -> [];

proplist_to_path([ { Key, Value } | Rest ]) -> 
    [ Key | proplist_to_path(Value) ++ proplist_to_path(Rest) ];

proplist_to_path(Node) -> [Node].

expire(Identifiers, IdDatabase, CacheDatabase) ->
    % io:format("expire: ~p~n", [Identifiers]),
    GetDigests = fun (Id, DigestsAcc) ->
        Digests = case dict:find(Id, IdDatabase) of
        {ok, Array} -> 
            Array;
        error -> 
            []
        end,
        Digests ++ DigestsAcc
    end,

    Digests = lists:foldr(GetDigests, [], Identifiers),


    RemoveKeys = fun (Digest, DatabaseAcc) ->
        dict:erase(Digest, DatabaseAcc)
    end,

    UpdatedCacheDatabase = 
        lists:foldr(RemoveKeys, CacheDatabase, Digests),
    UpdatedIdDatabase = 
        lists:foldr(RemoveKeys, IdDatabase, Identifiers),
    {UpdatedIdDatabase, UpdatedCacheDatabase}.

store(Cache, RequestDatabase, IdDatabase, CacheDatabase) ->
    Path = proplist_to_path(Cache#cache.request_filter),

    Digest = Cache#cache.digest,
    UpdatedCacheDatabase = dict:store(Digest, Cache, CacheDatabase),
    % io:format("store: ~p~n~n", [CacheDatabase]),

    UpdatedRequestDatabase = path_tree:store(RequestDatabase, Path, Digest),
    io:format("requestdb: ~p~n~n", [UpdatedRequestDatabase]),


    AppendDigest = fun (Id, DatabaseAcc) ->
        case dict:find(Id, DatabaseAcc) of
        {ok, Array} -> 
            dict:store(Id, [Digest|Array], DatabaseAcc);
        error -> 
            dict:store(Id, [Digest], DatabaseAcc)
       end
    end,
    UpdatedIdDatabase 
        = lists:foldr(AppendDigest, IdDatabase, Cache#cache.identifiers),
    
    {UpdatedRequestDatabase, UpdatedIdDatabase, UpdatedCacheDatabase}.


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

