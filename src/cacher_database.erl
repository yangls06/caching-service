%% @author Ryah Dahl <ry@tinycouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc Ensure that the relatively-installed dependencies are on the code
%%            loading path, and locate resources relative
%%            to this application's path.

-module(cacher_database).
-author('Ryah Dahl <ry@tinyclouds.org>').
-export([start/0, loop/3, serialize/1]).
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

    { exists, Digest, Client } ->
        case dict:is_key(Digest, CacheDatabase) of
        true ->
            Client ! yes;
        false ->
            Client ! no
        end,
        { RequestDatabase, IdDatabase, CacheDatabase };

    { find, RequestElements, Client } ->
        % io:format("find: ~p~n", [RequestElements]),
        Path = serialize(RequestElements),
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

% depth first serialization
serialize([]) -> [];

serialize([ { Key, Value } | Rest ]) -> 
    [ Key | serialize(Value) ++ serialize(Rest) ];

serialize(Node) -> [Node].

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

    UpdatedCacheDatabase = 
        lists:foldr(fun dict:erase/2, CacheDatabase, Digests),
    UpdatedIdDatabase = 
        lists:foldr(fun dict:erase/2, IdDatabase, Identifiers),
    {UpdatedIdDatabase, UpdatedCacheDatabase}.

store(Cache, RequestDatabase, IdDatabase, CacheDatabase) ->
    Path = serialize(Cache#cache.request_filter),

    Digest = Cache#cache.digest,
    UpdatedCacheDatabase = dict:store(Digest, Cache, CacheDatabase),
    % io:format("store: ~p~n~n", [CacheDatabase]),

    UpdatedRequestDatabase = path_tree:store(RequestDatabase, Path, Digest),
    % io:format("requestdb: ~p~n~n", [UpdatedRequestDatabase]),


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

