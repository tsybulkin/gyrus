-module(demo_ws_handler).
-export([init/2]).
-export([terminate/3]).
-export([websocket_info/3]).

-record(state, {}).

%% 

init(Req, _Opts) ->
  demo_game ! {subscribe, self()},
  {cowboy_websocket, Req, #state{}}.

terminate(_Reason, _Req, #state{}) ->
  demo_game ! {unsubscribe, self()}.

websocket_info({demo_game_move, Color, {X,Y}}, Req, State) ->
  {reply, {text, jsx:encode([demo_game_move, Color, X, Y])}, Req, State};
websocket_info({demo_game_over,Color,[{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4},{X5,Y5}]}, Req, State) ->
  {reply, {text, jsx:encode([demo_game_over, Color, X1,Y1, X2,Y2, X3,Y3, X4,Y4, X5,Y5])}, Req, State};
websocket_info({demo_game_over,draw}, Req, State) ->
  {reply, {text, jsx:encode([demo_game_over,draw])}, Req, State};
websocket_info(Msg, Req, State) ->
  throw({ws, Msg}).

