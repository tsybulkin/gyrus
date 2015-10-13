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
							Game_id = spawn(?MODULE,start_new_game,[Schedule,GS,bot,human]),
							WS ! {start_new_game, ID, state:init_state1(), blacks};
						whites ->
							Game_id = spawn(?MODULE,start_new_game,[Schedule,GS,human,bot]),
							WS ! {start_new_game, ID, state:init_state(), whites}
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
			WS ! {game_over, Game, Last_move, man_lost},
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,WS,CurrGames1,CurrPlayersNbr-1,Won+1,Draw,Lost,GamesDone+1);
			
		{game_over,Game,Last_move,draw} ->
			WS ! {game_over, Game, Last_move, draw},
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,WS,CurrGames1,CurrPlayersNbr-1,Won,Draw+1,Lost,GamesDone+1);		
		
		{game_over,Game,_Last_move,bot_game} ->
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,WS,CurrGames1,CurrPlayersNbr-2,Won,Draw,Lost,GamesDone+1)
		
		
	after
		1000 ->
			if CurrPlayersNbr < ?CONCURRENT_GAMES_LIMIT -> 
				spawn(?MODULE,start_new_game,[Schedule]),
				game_manager(Schedule,WS,CurrGames,CurrPlayersNbr+2,Won,Draw,Lost,GamesDone);
			true ->
				game_manager(Schedule,WS,CurrGames,CurrPlayersNbr,Won,Draw,Lost,GamesDone)
			end
	end.



start_new_game(Gyri,GS,blacks) -> %% run Agent vs. Bot
	% run game
	State = state:init_state(),
	run_game(Gyri,GS,State,blacks);
start_new_game(Gyri,GS,whites) -> %% run Agent vs. Bot
	% run game
	State = state:init_state1(),
	run_game(Gyri,GS,State,whites).



run_game(Gyri,GS,{Turn,_Board}=State,Color) -> 
	case color(Turn) =:= Color of
		true -> % your move
			Move = bot:get_move(Gyri,State),
			case change_state(State,Move) of
				blacks_won -> GS ! {game_over, self(), Move, man_lost};
				whites_won -> GS ! {game_over, self(), Move, man_lost};
				draw -> GS ! {game_over, self(), Move, draw};
				NextState -> GS ! {bot_move, self(), Move},
					run_game(Gyri,GS,NextState,Color)
			end;
		false->  % Opponent's move
			receive
				quit -> ok;		
				{move, Move} ->
					case change_state(State,Move) of
						blacks_won -> GS ! {game_over, self(), man_won};
						whites_won -> GS ! {game_over, self(), man_won};
						draw -> GS ! {game_over, self(), draw};
						NextState -> run_game(Gyri,GS,NextState,Color)
					end
			end
	end.



change_state({Turn,Board},{I,J}) ->
	case element(I,element(J,Board)) of
		e -> 
			io:format("~nNew ~p move:(~p,~p)~n",[Turn,I,J]),
			Row1 = erlang:delete_element(I,element(J,Board)),
			Board1 = erlang:delete_element(J,Board),
			case color(Turn) of
				whites -> Row2 = erlang:insert_element(I,Row1,w);
				blacks -> Row2 = erlang:insert_element(I,Row1,b)
			end,
			Next_state = {Turn+1, erlang:insert_element(J,Board1,Row2)};

		_ ->
			io:format("Illegal move: ~p~n",[{I,J}]),
			Next_state = illegal_move
	end,

	case lines:check_five(Next_state) of
		true -> 
			case color(Turn+1) of
				whites -> blacks_won;
				blacks -> whites_won
			end;
		_ when Turn =:= 99 -> draw;
		_ -> Next_state
	end.	




color(Turn) -> if (Turn rem 2) =:= 0 -> whites; true -> blacks end.
