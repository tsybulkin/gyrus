%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(game).
-export([game_manager/2,
		start_new_game/3
		]).

-define(CONCURRENT_GAMES_LIMIT, 11).



game_manager(Schedule,WS) -> game_manager(Schedule,WS,[],0,0,0,0,0).

game_manager(Schedule,WS,CurrGames,CurrPlayersNbr,Won,Draw,Lost,GamesDone) ->
	receive
		{new_game_request, Color, Level, ID} ->
			case CurrPlayersNbr < ?CONCURRENT_GAMES_LIMIT of
				true ->
					GS = self(),
					case Color of
						blacks ->
							Game_id = spawn(?MODULE,start_new_game,[Schedule,GS,whites]),
							WS ! {start_new_game, ID, state:init_state1(), blacks},
						whites ->
							Game_id = spawn(?MODULE,start_new_game,[Schedule,GS,blacks]),
							WS ! {start_new_game, ID, state:init_state(), whites},
					end,
					game_manager(Schedule,WS,[Game_id|CurrGames],CurrPlayersNbr+1,Won,Draw,Lost,GamesDone);
				
				false ->
					WS ! {too_many_players,ID},
					game_manager(Schedule,WS,CurrGames,CurrPlayersNbr,Won,Draw,Lost,GamesDone)
			end;

		{player_move, Game, Move} ->
			case lists:member(Game, CurrGames) of
				true -> Game ! {move, Move};

				false -> WS ! {game_not_exists, Game}
			end,
			game_manager(Schedule,WS,CurrGames,CurrPlayersNbr,Won,Draw,Lost,GamesDone);

		{bot_move, Game, Move} ->
			case lists:member(Game, CurrGames) of
				true -> WS ! {move, Game, Move};

				false -> Game ! quit
			end,
			game_manager(Schedule,WS,CurrGames,CurrPlayersNbr,Won,Draw,Lost,GamesDone);

		{connection_closed, Game} ->
			CurrGames1 = lists:delete(Game,CurrGames),
			Game ! quit,
			game_manager(Schedule,WS,CurrGames1,CurrPlayersNbr-1,Won+1,Draw,Lost,GamesDone+1);

		
		{game_over,Game,man_won} ->
			WS ! {game_over, Game, man_won},
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,WS,CurrGames1,CurrPlayersNbr-1,Won,Draw,Lost+1,GamesDone+1);
		
		{game_over,Game,draw} ->
			WS ! {game_over, Game, draw},
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,WS,CurrGames1,CurrPlayersNbr-1,Won,Draw+1,Lost,GamesDone+1);

		{game_over,Game,Last_move,man_lost} ->
			WS ! {game_over, Game, man_lost},
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,WS,CurrGames1,CurrPlayersNbr-1,Won+1,Draw,Lost,GamesDone+1);
			
		{game_over,Game,Last_move,draw} ->
			WS ! {game_over, Game, Last_move, draw},
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,WS,CurrGames1,CurrPlayersNbr-1,Won,Draw+1,Lost,GamesDone+1);		
		
		{game_over,Game,Last_move,bot_game} ->
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,WS,CurrGames1,CurrPlayersNbr-2,Won,Draw,Lost,GamesDone+1);
		
		
	after
		1000 ->
			if CurrPlayersNbr < ?CONCURRENT_GAMES_LIMIT -> 
				spawn(?MODULE,start_new_game,[Schedule]),
				game_manager(Schedule,WS,CurrGamesCurrPlayersNbr+2,Won,Draw,Lost,GamesDone);
			true ->
				game_manager(Schedule,WS,CurrGamesCurrPlayersNbr,Won,Draw,Lost,GamesDone)
			end
	end.



start_new_game(Schedule,GS,blacks) -> %% run Agent vs. Bot
	% run game
	State = state:init_state(),
	game(Schedule,GS,State);
start_new_game(Schedule,GS,whites) -> %% run Agent vs. Bot
	% run game
	State = state:init_state1(),
	game(Schedule,GS,State).



game(Girus,GS,{Turn,Board}) ->
	GS ! {game_over,self(),draw}.
	


