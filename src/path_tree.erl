%% @author Ryah Dahl <ry@tinycouds.org>
%% @copyright 2008 Ryah Dahl.

-module(path_tree).
-author('Ryah Dahl <ry@tinyclouds.org>').

-export([store/2, find/1]).
-export([test/0]).

% the key is the node's path in the tree
% Key = ["Path", "/friends", "Cookies", "Params", "skip", "20", "Headers", "Host", "four.livejournal.com"]
% The object is  { Key, Data, Subdirectories }
% Where Data = Cache#cache.digest
% Where Subdirectories = ["Accept"]

find(Query) ->
    MaximalPath = maximal_path(Query),
    % io:format("max path ~p~n", [MaximalPath]),
    case get_data(MaximalPath) of
    not_found ->
        error_in_maximal_path;
    nil ->
        not_found;
    Data ->
        {ok, Data}
    end.

% returns {replaced, Data} | ok 
store([], _) -> 
    error;
store(Path, Data) ->
    ParentDir = lists:sublist(Path, length(Path) - 1),
    Dir = lists:last(Path),
    % io:format("store path: ~p~nstore data: ~p~n~n", [Path, Data]),
    add_subdir(ParentDir, Dir),

    case get_data(Path) of
    not_found ->
        create_node(Path, Data, []),
        ok;
    nil ->
        create_node(Path, Data, []),
        ok;
    Data ->
        ok;
    Replaced ->
        set_data(Path, Data),
        {replaced, Replaced}
    end.

add_subdir(Path, SubDir) ->
    % io:format("add_subdir path: ~p~nadd_subdir subdir: ~p~n~n", [Path, SubDir]),
    append_subdir(Path, SubDir),
    case length(Path) of
    0 -> ok;
    _ -> 
        ParentDir = lists:sublist(Path, length(Path) - 1),
        Dir = lists:last(Path),
        add_subdir(ParentDir, Dir)
    end.

maximal_path(Query) ->
    maximal_path(Query, []).

maximal_path([], Travelled) ->
    % XXX check to see travelled is in the table?
    Travelled;
maximal_path(Query, Travelled) ->
    % io:format("maxpath query: ~p~nmaxpath travel: ~p~n", [Query, Travelled]),
    case get_subdirectories(Travelled)  of
    not_found ->
        lists:sublist(Travelled, length(Travelled) - 1);
    SubDirectories ->
        NotSubDir = fun(C) -> 
            not lists:member(C, SubDirectories) 
        end,
        case reject_head(NotSubDir, Query) of
        [] ->
            Travelled;
        [QueryHead|RemainingQuery] ->
            maximal_path(RemainingQuery, Travelled ++ [QueryHead])
        end
    end.

get_subdirectories(Path) ->
    case ets:lookup(path_tree, Path) of
    [] ->
        not_found;
    [{_, _, SubDirs}] ->
        SubDirs
    end.

set_subdirectories(Path, SubDirs) ->
    ets:update_element(path_tree, Path, {3, SubDirs}).

get_data(Path) ->
    case ets:lookup(path_tree, Path) of
    [] ->
        not_found;
    [{_, Data, _}] ->
        Data
    end.

set_data(Path, Data) ->
    ets:update_element(path_tree, Path, {2, Data}).
create_node(Path, Data, SubDirs) ->
    % io:format("create node: ~p~n", [Path]),
    ets:insert(path_tree, { Path, Data, SubDirs }).

% TODO implement subdirs using a bag table
% then wont require this extra look up step
append_subdir(Path, SubDir) ->
    % io:format("subdirs of ~p are ~p~n", [Path, get_subdirectories(Path)]),

    case get_subdirectories(Path) of
    not_found ->
        create_node(Path, nil, [SubDir]);
    SubDirectories ->
        set_subdirectories(Path, [SubDir|SubDirectories])
    end.

reject_head(_, []) ->
    [];
reject_head(Pred, [Elem|Rest]) ->
    case Pred(Elem) of
    true ->
        reject_head(Pred, Rest);
    false ->
        [Elem|Rest]
    end.

test() -> 
    ok.



