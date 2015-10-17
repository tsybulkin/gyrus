%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(game).
-export([start_link/0]).
-export([game_manager/1,
		game_manager/7,
    game_manager_call/1,
		start_new_game/5,
		change_state/2,
		color/1
		]).

-define(CONCURRENT_GAMES_LIMIT, 11).

start_link() ->
	Schedule = [],
	Pid = spawn_link(?MODULE, game_manager, [Schedule,[],0,0,0,0,0]),
	gyri:init_gyri(),
	true = register(game_manager, Pid),
	{ok, Pid}.

game_manager_call(Request) ->
	game_manager ! Request,
	receive Response -> Response end.

game_manager(Schedule) -> game_manager(Schedule,[],0,0,0,0,0).

game_manager(Schedule,CurrGames,CurrPlayersNbr,Won,Draw,Lost,GamesDone) ->
	receive
		{new_game_request, WS, Color, Level} ->
			io:format("~p~n", [{new_game_request, WS, Color, Level}]),
			case CurrPlayersNbr < ?CONCURRENT_GAMES_LIMIT of
				true ->
					GS = self(),
					case Color of
						blacks ->
							Game_id = spawn(?MODULE,start_new_game,[Schedule,GS,Level,whites,WS]),
							WS ! {start_new_game, state:init_state1(), blacks, Game_id};
						whites ->
							Game_id = spawn(?MODULE,start_new_game,[Schedule,GS,Level,blacks,WS]),
							WS ! {start_new_game, state:init_state(), whites, Game_id}
					end,
					game_manager(Schedule,[Game_id|CurrGames],CurrPlayersNbr+1,Won,Draw,Lost,GamesDone);
				
				false ->
					WS ! too_many_players,
					game_manager(Schedule,CurrGames,CurrPlayersNbr,Won,Draw,Lost,GamesDone)
			end;

		{player_move, WS, Game, Move} ->
			case lists:member(Game, CurrGames) of
				true -> Game ! {move, Move};

				false -> WS ! {game_not_exists, Game}
			end,
			game_manager(Schedule,CurrGames,CurrPlayersNbr,Won,Draw,Lost,GamesDone);

		{bot_move, WS, Game, Move} ->
			case lists:member(Game, CurrGames) of
				true -> WS ! {bot_move, Move};

				false -> Game ! quit
			end,
			game_manager(Schedule,CurrGames,CurrPlayersNbr,Won,Draw,Lost,GamesDone);

		{connection_closed, Game} ->
			CurrGames1 = lists:delete(Game,CurrGames),
			Game ! quit,
			game_manager(Schedule,CurrGames1,CurrPlayersNbr-1,Won+1,Draw,Lost,GamesDone+1);

		
		{game_over,WS,Game,man_won} ->
			WS ! {game_over, man_won},
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,CurrGames1,CurrPlayersNbr-1,Won,Draw,Lost+1,GamesDone+1);
		
		{game_over,WS,Game,draw} ->
			WS ! {game_over, draw},
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,CurrGames1,CurrPlayersNbr-1,Won,Draw+1,Lost,GamesDone+1);

		{game_over,WS,Game,Last_move,man_lost} ->
			WS ! {game_over, Last_move, man_lost},
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,CurrGames1,CurrPlayersNbr-1,Won+1,Draw,Lost,GamesDone+1);
			
		{game_over,WS,Game,Last_move,draw} ->
			WS ! {game_over, Game, Last_move, draw},
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,CurrGames1,CurrPlayersNbr-1,Won,Draw+1,Lost,GamesDone+1);
		
		{game_over,WS,Game,_Last_move,bot_game} ->
			CurrGames1 = lists:delete(Game,CurrGames),
			game_manager(Schedule,CurrGames1,CurrPlayersNbr-2,Won,Draw,Lost,GamesDone+1);

		%% TODO: delete this
		Err -> throw(Err)
		
%% 	after
%% 		1000 ->
%% 			if CurrPlayersNbr < ?CONCURRENT_GAMES_LIMIT ->
%% 				spawn(?MODULE,start_new_game,[Schedule]),
%% 				game_manager(Schedule,CurrGames,CurrPlayersNbr+2,Won,Draw,Lost,GamesDone);
%% 			true ->
%% 				game_manager(Schedule,CurrGames,CurrPlayersNbr,Won,Draw,Lost,GamesDone)
%% 			end
	end.



start_new_game(Schedule,GS,Level,blacks,WS) -> %% run Agent vs. Bot
	% bot plays for blacks
	State = state:init_state(),
	run_game(Schedule,GS,Level,State,blacks,WS);
start_new_game(Schedule,GS,Level,whites,WS) -> %% run Agent vs. Bot
	% bot plays for whites
	State = state:init_state1(),
	run_game(Schedule,GS,Level,State,whites,WS).



run_game(Schedule,GS,Level,{Turn,_Board}=State,Color,WS) ->
	case color(Turn) =:= Color of
		true -> % your move
			Move = bot:get_move(Level,State),
			io:format("Bot move:~p, State:~p~n",[Move,State]),
			%Move = rand:rand(State),
			case change_state(State,Move) of
				blacks_won -> GS ! {game_over, WS, self(), Move, man_lost}, gyri:save_gyri();
				whites_won -> GS ! {game_over, WS, self(), Move, man_lost}, gyri:save_gyri();
				draw -> GS ! {game_over, WS, self(), Move, draw}, gyri:save_gyri();
				NextState -> GS ! {bot_move, WS, self(), Move},
					run_game(Schedule,GS,Level,NextState,Color,WS)
			end;
		false->  % Opponent's move
			receive
				quit -> ok;		
				{move, Move} ->
					io:format("Human move:~p, State:~p~n",[Move,State]),
					case change_state(State,Move) of
						blacks_won -> GS ! {game_over, WS, self(), man_won}, gyri:save_gyri();
						whites_won -> GS ! {game_over, WS, self(), man_won}, gyri:save_gyri();
						draw -> GS ! {game_over, WS, self(), draw}, gyri:save_gyri();
						NextState -> run_game(Schedule,GS,Level,NextState,Color,WS)
					end
			end
	end.



change_state({Turn,Board},{I,J}) ->
	case moves:legal_move({I,J},Board) of
		true ->
			%io:format("~nNew ~p move:(~p,~p)~n",[Turn,I,J]),
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
		false when Turn =:= 99 -> draw;
		false -> Next_state
	end.	




color(Turn) -> if (Turn rem 2) =:= 0 -> whites; true -> blacks end.
