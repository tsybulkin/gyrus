-module(ws_handler).
-export([init/2]).
-export([websocket_info/3]).
-export([websocket_handle/3]).

-record(state, {board}).

handle_client_msg(<<"init">>, Req, State) ->
  {reply, {text, <<"ok">>}, Req, State}.

%% 

init(Req, _Opts) ->
  BoardId = cowboy_req:binding(board, Req),
  GamePid = dummy_game_manager:get_game(BoardId),
  dummy_game:subscribe(GamePid, self()),
  GameData = dummy_game:get_data(GamePid),
  erlang:send(self(), {data, GameData}),
  {cowboy_websocket, Req, #state{board = BoardId}}.

websocket_info({data, Board}, Req, State) ->
  {reply, {text, jsx:encode([data,Board])}, Req, State};
websocket_info(Msg, Req, State) ->
  throw({ws, Msg}).
%% websocket_info(_Msg, Req, State) ->
%%   {ok, Req, State}.

websocket_handle({text, Bin}, Req, State) ->
  Msg = jsx:decode(Bin),
  handle_client_msg(Msg, Req, State).

