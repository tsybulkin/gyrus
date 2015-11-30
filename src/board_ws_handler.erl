-module(board_ws_handler).
-export([init/2]).
-export([terminate/3]).
-export([websocket_info/3]).
-export([websocket_handle/3]).

-record(state, {game_pid}).

handle_client_msg([<<"player_move">>,X,Y], Req, S=#state{game_pid = GamePid}) ->
  Reply = case game:game_manager_call({player_move, self(), GamePid, {X,Y}}) of
    {bot_move,{X1,Y1}} ->
      [bot_move, X1, Y1];
    {game_over,[{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4},{X5,Y5}],man_lost} ->
      [game_over, man_lost, X1,Y1, X2,Y2, X3,Y3, X4,Y4, X5,Y5];
    {game_over,[{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4},{X5,Y5}],man_won} ->
      [game_over, man_won, X1,Y1, X2,Y2, X3,Y3, X4,Y4, X5,Y5];
    {game_over, draw} ->
      [game_over, draw]
  end,
  {reply, {text, jsx:encode(Reply)}, Req, S}.

%% 

init(Req, _Opts) ->
  ColorBin = cowboy_req:binding(color, Req),
  LevelBin = cowboy_req:binding(level, Req),
  Color = binary_to_atom(ColorBin, latin1),
  Level = binary_to_atom(LevelBin, latin1),
  {start_new_game,_,Color,GamePid} = game:game_manager_call({new_game_request, self(), Color, Level}),
  {cowboy_websocket, Req, #state{game_pid = GamePid}}.

terminate(_Reason, _Req, #state{game_pid = Game}) ->
  game:game_manager_call({connection_closed, Game}).

websocket_info({bot_move, {X,Y}}, Req, State) ->
  {reply, {text, jsx:encode([bot_move, X, Y])}, Req, State};
websocket_info({counters,Won,Draw,Lost,GamesDone}, Req, State) ->
  {reply, {text, jsx:encode([counters,Won,Draw,Lost,GamesDone])}, Req, State};
websocket_info(Msg, Req, State) ->
  throw({ws, Msg}).

websocket_handle({text, Bin}, Req, State) ->
  Msg = jsx:decode(Bin),
  handle_client_msg(Msg, Req, State).

