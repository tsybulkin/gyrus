-module(dummy_game_manager).
-behaviour(gen_server).
-compile([export_all]).

-record(state, {games = start_fake_games()}).

start_fake_games() ->
  {ok, Pid} = dummy_game:start_link(0),
  timer:apply_after(5000, dummy_game, move, [Pid, b, 1, 1]),
  [{0, Pid}].

start_link() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_game(BoardId) when is_binary(BoardId) ->
  gen_server:call(?MODULE, {get_game,BoardId}).

%% callbacks

init(_Args) ->
  {ok, #state{}}.

handle_call({get_game,<<"new_game">>}, _From, State) ->
  {reply, [], State};
handle_call({get_game, GameIdBin}, _From, State) when is_binary(GameIdBin) ->
  GameId = binary_to_integer(GameIdBin),
  GamePid = proplists:get_value(GameId, State#state.games),
  {reply, GamePid, State}.
