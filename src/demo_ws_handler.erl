-module(demo_ws_handler).
-export([init/2]).
-export([terminate/3]).
-export([websocket_info/3]).

-record(state, {}).

%% 

init(Req, _Opts) ->
  %{start_new_game,_,Color,GamePid} = game:game_manager_call({new_game_request, self(), Color, Level}),
  {cowboy_websocket, Req, #state{}}.

terminate(_Reason, _Req, #state{}) ->
  ok. %game:game_manager_call({connection_closed, Game}).

websocket_info({bot_move, {X,Y}}, Req, State) ->
  {reply, {text, jsx:encode([bot_move, X, Y])}, Req, State};
websocket_info({counters,Won,Draw,Lost,GamesDone}, Req, State) ->
  {reply, {text, jsx:encode([counters,Won,Draw,Lost,GamesDone])}, Req, State};
websocket_info(Msg, Req, State) ->
  throw({ws, Msg}).

