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
        case Req:get_header_value("if-none-match") of
        undefined -> 
            find(Req);
        Digest ->
            exists(Req, Digest)
        end;
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

exists(Req, Digest) ->
    cacher_database ! { exists, Digest, self() },
    receive
    yes ->  
        Req:respond({304, [], []}); % return Not Modified
    no ->
        find(Req)
    end.

find(Req) ->
    RequestElements = request_elements(Req),
    % io:format("find. request elements = ~p~n", [RequestElements]),
    cacher_database ! { find, RequestElements, self() },
    receive
    {ok,  Cache} ->
        %io:format("found content type: ~p~ndata: ~p~n", [ Cache#cache.content_type,  Cache#cache.data ]),
        Headers = [{"ETag", Cache#cache.digest}],
        Req:ok({Cache#cache.content_type, Headers, Cache#cache.data});
    not_found -> 
        Req:not_found();
    _ ->
        Req:respond({500, [], []})
    end.

store(Req) ->
    Body = Req:recv_body(),
    XCacheIdentifiers = Req:get_header_value("x-cache-identifiers"),
    Cache = #cache{
                  data = Body,
                digest = digest(Body),
           identifiers = parse_identifiers(XCacheIdentifiers),
          content_type = Req:get_header_value("content-type"),
        request_filter = request_elements(Req)
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
    Headers = [{ToLowerString(Key), Value} || {Key,Value} <- MochiList], 
    ToDelete = [ "cache-control"
               , "connection"
               , "content-encoding"
               , "content-length"
               , "content-type"
               , "etag"
               , "location"
               , "x-cache-identifiers"
               ],
    FilteredHeaders = lists:foldr(fun proplists:delete/2, Headers, ToDelete),
    [ { "Path"   , Req:get(path) }
    , { "Cookies", lists:sort(Req:parse_cookie()) }
    , { "Params" , lists:sort(Req:parse_qs()) }
    , { "Headers", lists:sort(FilteredHeaders) }
    ]. 

parse_identifiers(String) -> 
    string:tokens(String, " ,").

digest(Body) ->
    mochihex:to_hex(crypto:sha(Body)).

test_digest() ->
    crypto:start(),
    digest(<<"hello world">>),
    ok.

test() ->
    ok = test_digest(),
    ok.

