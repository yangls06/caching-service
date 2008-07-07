%% @author Ryah Dahl <ry@tinycouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc Ensure that the relatively-installed dependencies are on the code
%%            loading path, and locate resources relative
%%            to this application's path.

-module(cacher_database).
-author('Ryah Dahl <ry@tinyclouds.org>').
-export([start/0, loop/0, profile/2, create_tables/0, find/1]).
-export([test/0]).
-include_lib("cacher.hrl").

start() ->
    create_tables(),
    register(?MODULE, spawn(?MODULE, loop, [])).

create_tables() ->
    ets:new(path_tree,    [public, named_table, ordered_set]),
    create_root_node(),
    ets:new(cache_table, [public, named_table, ordered_set]),
    ets:new(id_table,    [public, named_table, bag]).

loop() ->
    receive
    { store, Cache } ->
        store(Cache);

    { expire, Identifiers, Client } ->
        expire(Identifiers);

    { exists, Digest, Client } ->
        case ets:member(cache_table, Digest) of
        true  -> Client ! true;
        false -> Client ! false
        end;

    { find, Path, Client } ->
        case find(Path) of 
        not_found   -> Client ! not_found;
        {ok, Cache} -> Client ! {ok, Cache}
        end
    end,
    loop(). 

store(Cache) ->
    Path = Cache#cache.request_path,
    Digest = Cache#cache.digest,

    case create_node(Path, Digest) of
    false ->
        Replaced = get_data(Path),
        set_data(Path, Digest),
        ets:delete(cache_table, Replaced);
    true ->
        ParentDir = parent_dir(Path),
        add_dir(ParentDir),
        ok
    end,

    ets:insert(cache_table, {Digest, Cache}), 

    InsertIdTable = fun (Id) ->
        ets:insert(id_table, {Id, Digest})
    end,
    lists:foreach(InsertIdTable, Cache#cache.identifiers).

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


% the key is the node's path in the tree
% Key = ["Path", "/friends", "Cookies", "Params", "skip", "20", "Headers", "Host", "four.livejournal.com"]
% The object is  { Key, Data }
% Where Data = Cache#cache.digest

find(Query) ->
    Path = maximal_path(Query, []),
    case get_data(Path) of
    not_found ->
        error_in_maximal_path;
    nil ->
        not_found;
    Digest -> 
        case ets:lookup(cache_table, Digest) of
        [] ->
            not_found;
        [{_, Cache}] ->
            {ok, Cache}
        end
    end.


add_dir([]) -> 
    % created at startup
    ok;
add_dir(Dir) ->
    % io:format("add_subdir path: ~p~nadd_subdir subdir: ~p~n~n", [Path, SubDir]),
    case create_node(Dir, nil) of
    false ->
        % dir already exists in database
        ok; 
    true ->
        ParentDir = parent_dir(Dir),
        add_dir(ParentDir)
    end.

maximal_path([], Travel) ->
    Travel;
maximal_path([Query|RestOfQuery], Travel) ->
    NewTravel = Travel ++ [Query],
    case ets:member(path_tree, NewTravel) of
    false ->
       maximal_path(RestOfQuery, Travel); 
    true ->
       maximal_path(RestOfQuery, NewTravel)
    end.

get_data(Path) ->
    case ets:lookup(path_tree, Path) of
    [] ->
        not_found;
    [{_, Data}] ->
        Data
    end.

set_data(Path, Data) ->
    ets:update_element(path_tree, Path, {2, Data}).

create_root_node() ->
    create_node([], nil).

create_node(Path, Data) ->
    ets:insert_new(path_tree, {Path, Data}).

parent_dir(Path) ->
    lists:sublist(Path, length(Path) - 1).

% generate a random n element path
rpath(0) ->
    [];
rpath(N) ->
    [random:uniform()|rpath(N-1)].

rCache(N) ->
    #cache{
        data = "hello world", 
        digest = "2aae6c35c94fcfb415dbe95f408b9ce91ee846ed",
        identifiers = ["123", "321"],
        content_type = "text/plain",
        request_path = rpath(N)
    }.

profile(N, M) ->
    create_tables(),
    store(rCache(M)),
    fprof:apply(?MODULE, find, [rpath(N)]),
    fprof:profile(),
    fprof:analyse().


test() ->
    ok.

