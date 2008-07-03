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
    RequestElements = request_elements(Req),
    {RequestFilter, ContentType, Identifiers} 
        = filter_request_elements(RequestElements),
    % io:format("stored. request filter = ~p~n", [RequestFilter]),
    Body = Req:recv_body(),
    Cache = #cache{ request_filter = RequestFilter,
                    data = Body,
                    content_type = ContentType,
                    identifiers = Identifiers,
                    digest = filter_digest(Body)
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

    Params = Req:parse_qs(),
    Cookies = Req:parse_cookie(),

    H1 = if 
    Params == [] -> H;
    true -> [{"params", Params} | H]
    end,
    H2 = if 
    Cookies == [] -> H1;
    true -> [{"cookies", Cookies} | H1]
    end,

    [{"path", Req:get(path)} | lists:sort(H2)].

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

parse_identifiers(String) -> 
    string:tokens(String, " ,").

filter_digest(Body) ->
    mochihex:to_hex(crypto:sha(Body)).

test_filter_digest() ->
    crypto:start(),
    filter_digest(<<"hello world">>),
    ok.

test() ->
    ok = test_filter_digest(),
    ok.

