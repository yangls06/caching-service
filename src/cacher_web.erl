%% @author Ryah Dahl <ry@tinyclouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc Web server for cacher.

-module(cacher_web).
-author('Ryah Dahl <ry@tinyclouds.org>').

-export([start/1, stop/0, loop/1]).

%% External API

start(Options) ->
    cacher_database:start(),

    {DocRoot, Options1} = get_option(docroot, Options),

    Loop = fun (Req) ->
                   ?MODULE:loop(Req)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            find(Req);
        'POST' ->
            case Path of
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
    Req:respond({501, [], []}).

expire(Req) ->
    Req:respond({501, [], []}).

store(Req) ->
    Params = Req:parse_qs(),
    Cookies = Req:parse_cookie(),
    Path = Req:get(path),
    HeaderList = mochiweb_headers:to_list(Req:get(headers)),
    RequestFilter = lists:sort([ {'Path', Path}
                               , {'Params', Params}
                               , {'Cookies', Cookies} 
                               | HeaderList ]),
    Data = "data",
    Ids = [123],
    ContentType = "text/html",

    cacher_database ! { store, RequestFilter, Ids, ContentType, Data }, 
    Req:ok({"text/plain", [], 
      io_lib:format("stored: ~p ~n", [RequestFilter])
    }).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
