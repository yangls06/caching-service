%% @author Ryah Dahl <ry@tinyclouds.org>
%% @copyright 2008 Ryah Dahl.

%% @doc Web server for cacher.

-module(cacher_web).
-author('Ryah Dahl <ry@tinyclouds.org>').
-export([start/1, stop/0, loop/1]).
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
    HeaderList = header_list(Req),
    ContentType = proplists:get_value('Content-Type', HeaderList),
    NotReserved = fun 
      ({'Content-Type', _}) -> false;
      ({'Content-Length', _}) -> false;
      (_) -> true
    end,
    RequestFilter = lists:sort(lists:filter(NotReserved, HeaderList)),

    Data = "data",
    Ids = [123],

    Cache = #cache{ request_filter = RequestFilter, 
                    ids = Ids, 
                    content_type =ContentType, 
                    data = Data 
                  },
    cacher_database ! { store, Cache }, 

    Req:ok({"text/plain", [], 
      io_lib:format("stored: ~p ~n", [RequestFilter])
    }).


header_list(Req) ->
  HeaderList = mochiweb_headers:to_list(Req:get(headers)),

  ValueNotEmpty = fun
    ({_,  []}) -> false;
    (_) -> true        
  end,
  ExtraElements = lists:filter( ValueNotEmpty,
    [{'Path', Req:get(path)}, 
     {'Params', Req:parse_qs()}, 
     {'Cookies', Req:parse_cookie()}]
  ),
  ExtraElements ++ HeaderList.
