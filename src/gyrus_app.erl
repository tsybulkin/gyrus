-module(gyrus_app).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),
  ok = application:start(sync),
  ok = application:start(gyrus).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
      {'_', [
        {"/", cowboy_static, {file, "priv/index.html"}},
        {"/static/[...]", cowboy_static, {dir, "priv/static"}},
        {"/new_game/:color/:level", board_ws_handler, []},
        {"/demo_game", demo_ws_handler, []},
        {"/board/:board", ws_handler, []},
        {"/info", info_handler, []}
      ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{ip,{127,0,0,1}},{port, 8080}],
      [{env, [{dispatch, Dispatch}]}]),

    gyrus_sup:start_link().

stop(_State) ->
    ok.
