%% @author Ryah Dahl <ry@tinyclouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc Web server for cacher.

-module(cacher_web).
-author('Ryah Dahl <ry@tinyclouds.org>').
-export([start/1, stop/0, loop/1]).
-export([test/0]).
-include_lib("cacher.hrl").
 

%% External API

start(Options) ->
    cacher_database:start(),

    Loop = fun (Req) ->
        ?MODULE:loop(Req)
    end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    case Req:get(method) of
    'GET' ->
        find(Req);
    'POST' ->
        case Req:get(path) of
            "/_expire" ->
                expire(Req);
            _ ->
                store(Req)
        end;
    _ ->
        Req:respond({501, [], []})
    end.

%% Internal API

find(Req) ->
    RequestElements = request_elements(Req),
    % io:format("find. request elements = ~p~n", [RequestElements]),
    cacher_database ! { find, RequestElements, self() },
    receive
    {ok,  Cache} -> 
        %io:format("found content type: ~p~ndata: ~p~n", [ Cache#cache.content_type,  Cache#cache.data ]),
        ETag = Cache#cache.digest,
        Headers = [{"ETag", ETag}],
        Req:ok({Cache#cache.content_type, Headers, Cache#cache.data});
    not_found -> 
        Req:not_found();
    _ ->
        Req:respond({500, [], []})
    end.

store(Req) ->
    RequestElements = request_elements(Req),
    {RequestFilter, ContentType, Identifiers} 
        = filter_request_elements(RequestElements),
    % io:format("stored. request filter = ~p~n", [RequestFilter]),
    Cache = #cache{ request_filter = RequestFilter,
                    data = Req:recv_body(),
                    content_type = ContentType,
                    identifiers = Identifiers,
                    digest = filter_digest(RequestFilter)
                  },
    %io:format("cache digest = ~p~n", [Cache#cache.digest]),
    cacher_database ! {store, Cache},
    Req:ok({"text/plain", [], "stored"}).

expire(Req) ->
    IdentifierString = proplists:get_value("identifiers", Req:parse_qs()),
    Identifiers = parse_identifiers(IdentifierString),
    cacher_database ! { expire, Identifiers, self() },
    receive
    ok -> 
        Req:ok({"text/plain", [], "ok"});
    _ ->
        Req:respond({500, [], []})
    end.



request_elements(Req) ->
    MochiList = mochiweb_headers:to_list(Req:get(headers)),
    ToLowerString = fun(Key) ->
        String = if is_atom(Key) ->
            atom_to_list(Key);
        true ->
            Key
        end,
        string:to_lower(String)
    end,
    H = [ {ToLowerString(Key), Value} || {Key,Value} <- MochiList],
    ValueNotEmpty = fun
    ({_,  []}) -> false;
    (_) -> true        
    end,
    ExtraElements = lists:filter( ValueNotEmpty, [
        {"path", Req:get(path)},  % TODO normalize path
        {"params", Req:parse_qs()}, 
        {"cookies", Req:parse_cookie()}
    ]),
    lists:sort(ExtraElements ++ H).

filter_request_elements(RequestElements) ->
    ContentType = proplists:get_value("content-type", RequestElements),
    Identifiers = parse_identifiers(
        proplists:get_value("x-cache-identifiers", RequestElements)
    ),
    R0 = proplists:delete("host", RequestElements),
    R1 = proplists:delete("content-type", R0),
    R2 = proplists:delete("x-cache-identifiers", R1),
    R3 = proplists:delete("content-length", R2),
    R4 = proplists:delete("connection", R3),
    % reject this for now
    % TODO change accept value into an array?
    R5 = proplists:delete("accept", R4),
    RequestFilter = R5,
    {RequestFilter, ContentType, Identifiers}.

parse_identifiers(String) -> string:tokens(String, " ,").

filter_digest(RequestFilter) ->
    %crypto:start(),
    Path = cacher_database:proplist_to_path(RequestFilter),
    %io:format("path = ~p~n", [Path]),
    StringRepresentation = string:join(Path, "::"),
    mochihex:to_hex(crypto:sha(StringRepresentation)).

test_filter_digest() ->
    crypto:start(),
    filter_digest([ {"A", "B"} ]),
    ok.

test() ->
    ok = test_filter_digest(),
    ok.

