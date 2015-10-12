-module(dummy_game).
-behaviour(gen_server).
-compile([export_all]).

start_link(GameId) ->
  gen_server:start_link(?MODULE, GameId, []).

subscribe(GamePid, SubscriberPid) ->
  gen_server:call(GamePid, {subscribe, SubscriberPid}).

get_data(GamePid) ->
  gen_server:call(GamePid, get_data).

move(GamePid, Color, X, Y) ->
  gen_server:call(GamePid, {move, Color, X, Y}).

move(GamePid, Color, X0, Y0, X1, Y1) ->
  gen_server:call(GamePid, {move, Color, X0, Y0, X1, Y1}).

%%

-record(state, {id, board = {
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
  {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}}, subscribers=sets:new(), whites, blacks}).

init(GameId) ->
  {ok, #state{id = GameId}}.

handle_call({move, Color, X, Y}, _From, S=#state{board = Board}) when Color==w; Color==b ->
  case get_pos(X,Y,Board) == e of
    true ->
      Board1 = set_pos(X,Y, Color, Board),
      S1 = S#state{board = Board1},
      notify_subscribers(S1),
      {reply, ok, S1};
    false ->
      {reply, invalid_move, S}
  end;

handle_call({move, Color, X0, Y0, X1, Y1}, _From, S) when Color==w; Color==b ->
  case get_pos(X1,Y1,S#state.board) == e of
    true ->
      Board1 = set_pos(X0, Y0, e, S#state.board),
      Board2 = set_pos(X1,Y1, Color, Board1),
      S1 = S#state{board = Board2},
      notify_subscribers(S1),
      {reply, ok, S1};
    false ->
      {reply, invalid_move, S}
  end;

handle_call({subscribe, Pid}, _From, S) ->
  {reply, ok, S#state{subscribers = sets:add_element(Pid, S#state.subscribers)}};

handle_call(get_data, _From, S) ->
  {reply, board2list(S#state.board), S}.

%%

board2list(Board) ->
  lists:map(fun(T) -> tuple_to_list(T) end, tuple_to_list(Board)).

get_pos(X, Y, Board) ->
  element(X,element(Y,Board)).

set_pos(X, Y, Val, Board) ->
  setelement(Y, Board, setelement(X, element(Y,Board), Val)).

notify_subscribers(#state{board = Board, subscribers = Pids}) ->
  lists:foreach(fun(Pid) -> Pid ! {data, board2list(Board)} end, sets:to_list(Pids)).

