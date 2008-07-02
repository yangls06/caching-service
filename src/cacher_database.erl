%% @author Ryah Dahl <ry@tinycouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc Ensure that the relatively-installed dependencies are on the code
%%            loading path, and locate resources relative
%%            to this application's path.

-module(cacher_database).
-author('Ryah Dahl <ry@tinyclouds.org>').
-export([start/0, loop/3]).
-export([test/0]).
-include_lib("cacher.hrl").

start() ->
    RequestDatabase = [],
    IdDatabase = dict:new(),
    CacheDatabase = dict:new(),
    Pid = spawn(cacher_database, loop, [RequestDatabase, IdDatabase, CacheDatabase]),
    register(cacher_database, Pid).

loop(RequestDatabase, IdDatabase, CacheDatabase) ->
    receive
        { store, Cache } ->
            Path = proplist_to_path(Cache#cache.request_filter),

            Digest = cache_digest(Cache),
            %io:format("digest: ~p~n", [Digest]),
            UpdatedCacheDatabase = dict:store(Digest, Cache, CacheDatabase),
            io:format("store: ~p~n~n", [CacheDatabase]),

            UpdatedRequestDatabase = path_tree:store(RequestDatabase, Path, Digest),
            
            UpdateCaches = fun (Id, DatabaseAcc) ->
                case dict:find(Id, DatabaseAcc) of
                    {ok, Array} -> 
                        dict:store(Id, [Digest|Array], DatabaseAcc);
                    error -> 
                        dict:store(Id, [Digest], DatabaseAcc)
               end
            end,
            UpdatedIdDatabase = lists:foldr(UpdateCaches, IdDatabase, Cache#cache.identifiers),

            loop(UpdatedRequestDatabase, UpdatedIdDatabase, UpdatedCacheDatabase);

        { expire, Identifiers, Client } ->
            % io:format("expire: ~p~n", [Identifiers]),
            GetDigests = fun (Id, DigestsAcc) ->
                Digests = 
                case dict:find(Id, IdDatabase) of
                    {ok, Array} -> Array;
                    error -> []
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
            
            Client ! ok,

            loop(RequestDatabase, UpdatedIdDatabase, UpdatedCacheDatabase);

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
            loop(RequestDatabase, IdDatabase, CacheDatabase);
        Unknown ->
            io:format("unknown message to database: ~p~n", [Unknown]),
            loop(RequestDatabase, IdDatabase, CacheDatabase)
    end.

% The RequestDatabase looks like this:
% [ {"Accept", 
%     [ { "text/html",
%

proplist_to_path([]) -> 
    [];
proplist_to_path([ { Key, Value } | Rest ]) -> 
    [ Key | proplist_to_path(Value) ++    proplist_to_path(Rest) ];
proplist_to_path(Node) -> [Node].



cache_digest(Cache) ->
    crypto:start(),
    Path = proplist_to_path(Cache#cache.request_filter),
    StringRepresentation = string:join(Path, "::"),
    crypto:sha(StringRepresentation).

test_cache_digest() ->
    crypto:start(),
    Cache = #cache{ request_filter = [ {"A", "B"} ] },
    cache_digest(Cache),

    ok.

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
    ok = test_cache_digest(),
    ok.

