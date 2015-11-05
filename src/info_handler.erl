-module(info_handler).

-export([init/2]).

init(Req, _Opts) ->
    game_manager ! {get_counters, self()},
    receive 
        {counters,Won,Draw,Lost,GamesDone} ->
          Json = jsx:encode([counters,Won,Draw,Lost,GamesDone]),
          Req2 = cowboy_req:reply(200, [
              {<<"content-type">>, <<"text/plain">>}
          ], Json, Req),
          {ok, Req2, nostate}
        after 1000 ->
            {ok, Req, nostate}
    end.
