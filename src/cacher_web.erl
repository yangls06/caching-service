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

expire(Req) ->
    IdentifierString = proplists:get_value("identifiers", Req:parse_qs()),
    Identifiers = parse_identifiers(IdentifierString),
    cacher_database ! { expire, Identifiers, self() },
    receive
    {ok, Caches} -> 
        Req:ok({"text/plain", [], io_lib:format("expired: ~p", [Caches])});
    _ ->
        Req:respond({500, [], []})
    end.


store(Req) ->
    RequestFilter = request_elements(Req),
    Cache = #cache{ request_filter = RequestFilter,
                    data = <<"">>,
                    identifiers = <<"">>,
                    digest = filter_digest(RequestFilter)
                  },

    Callback = fun(Next) -> 
        multipart_callback(Next, Cache, nil) 
    end,
    mochiweb_multipart:parse_multipart_request(Req, Callback),
    Req:ok({"text/plain", [], "stored"}).


request_elements(Req) ->

    HeaderList = [ string:to_lower({atom_to_list(Key), Value}) 
                     || {Key,Value} <- header_list(Req) ],

    NotReserved = fun ({Key, _}) ->
        case Key of
        "content-type" -> false;
        "content-length" -> false;
        _ -> true
        end
    end,
    lists:sort(lists:filter(NotReserved, HeaderList)).

update_multipart_state(Headers, Cache) ->
    case proplists:get_value("content-disposition", Headers) of
    {"form-data", FormData} ->
          case proplists:get_value("name", FormData) of
          "identifiers" ->
              UpdatedCache = Cache#cache{identifiers = <<"">>},
              {identifiers, UpdatedCache};
          "cache" ->
              {CType, _} = proplists:get_value("content-type", Headers),
              UpdatedCache = Cache#cache{ content_type = CType, 
                                          data = <<"">>},
              {cache, UpdatedCache};
          _ ->
              {unknown, Cache}
          end;
    _ ->  {unknown, Cache}
    end.

multipart_callback(Next, Cache, LastName) ->
    {Name, UpdatedCache} =
    case Next of
    {headers, Headers} ->
        update_multipart_state(Headers, Cache);

    {body, Data} ->
        case LastName of
        identifiers ->
            Identifiers = <<(Cache#cache.identifiers)/binary, Data/binary>>,
            % io:format("got ids: ~p~n", [Identifiers]),
            {identifiers, Cache#cache{identifiers = Identifiers}};
        cache ->
            %UpdatedData = Data,
            UpdatedData = <<(Cache#cache.data)/binary, Data/binary>>,
            % io:format("got cache: ~p~n", [UpdatedData]),
            {cache, Cache#cache{data =  UpdatedData}};
        X -> {X, Cache}
        end;

    body_end -> 
        if Cache#cache.data =/= "" andalso Cache#cache.identifiers =/= "" -> 
            %IDs = parse_identifiers(binary_to_list(Cache#cache.identifiers)),
            IDs = parse_identifiers(binary_to_list(Cache#cache.identifiers)),
            % io:format("ids: ~p~n", [IDs]),
            % io:format("cache: ~p~n", [Cache#cache.data]),
            cacher_database ! { store, Cache#cache{identifiers = IDs} };
        true -> ok
        end,
        {nil, Cache};

    _ -> 
        {nil, Cache}
    end,
    fun(N) -> 
        multipart_callback(N, UpdatedCache, Name) 
    end.


parse_identifiers(String) -> string:tokens(String, " ,").

header_list(Req) ->
    MochiList = mochiweb_headers:to_list(Req:get(headers)),
    HeaderList = [ string:to_lower({atom_to_list(Key), Value}) || {Key,Value} <- MochiList],

    ValueNotEmpty = fun
    ({_,  []}) -> false;
    (_) -> true        
    end,
    ExtraElements = lists:filter( ValueNotEmpty, [
        {"path", Req:get(path)},  % TODO normalize path
        {"params", Req:parse_qs()}, 
        {"cookies", Req:parse_cookie()}
    ]),
    ExtraElements ++ HeaderList.

filter_digest(RequestFilter) ->
    %crypto:start(),
    Path = cacher_database:proplist_to_path(RequestFilter),
    StringRepresentation = string:join(Path, "::"),
    crypto:sha(StringRepresentation).

test_filter_digest() ->
    crypto:start(),
    filter_digest([ {"A", "B"} ]),
    ok.

test() ->
    ok = test_filter_digest(),
    ok.

