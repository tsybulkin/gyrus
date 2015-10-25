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
		start_new_game/5, start_new_bot_game/2,
		change_state/2,
		color/1
		]).

-define(HUMAN_BOT_GAMES_LIMIT, 2).
-define(BOT_BOT_GAMES_LIMIT, 4).

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

game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone) ->
	%io:format("Human-Bot games: ~p  Bot-Bot games: ~p Total games played: ~p~n",
	%	[length(Human_bot_games),Bot_bot_gameNBR, GamesDone]),
	receive
		{new_game_request, WS, Color, Level} ->
			io:format("~p~n", [{new_game_request, WS, Color, Level}]),
			case length(Human_bot_games) < ?HUMAN_BOT_GAMES_LIMIT of
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
					game_manager(Schedule,[Game_id|Human_bot_games],Bot_bot_gameNBR,Won,Draw,Lost,GamesDone);
				
				false ->
					WS ! {too_many_players,length(Human_bot_games)+Bot_bot_gameNBR},
					game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone)
			end;

		{player_move, WS, Game, Move} ->
			case lists:member(Game, Human_bot_games) of
				true -> Game ! {move, Move};

				false -> WS ! {game_not_exists, Game}
			end,
			game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone);

		{bot_move, WS, Game, Move} ->
			case lists:member(Game, Human_bot_games) of
				true -> WS ! {bot_move, Move};

				false -> Game ! quit
			end,
			game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone);

		{connection_closed, Game} ->
			Human_bot_games1 = lists:delete(Game,Human_bot_games),
			case length(Human_bot_games) =:= length(Human_bot_games1) of
				true -> game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone);
				false->
					Game ! quit,
					game_manager(Schedule,Human_bot_games1,Bot_bot_gameNBR,Won+1,Draw,Lost,GamesDone+1)
			end;

		
		{game_over,WS,Game,man_won} ->
			WS ! {game_over, man_won},
			Human_bot_games1 = lists:delete(Game,Human_bot_games),
			game_manager(Schedule,Human_bot_games1,Bot_bot_gameNBR,Won,Draw,Lost+1,GamesDone+1);
		
		{game_over,WS,Game,draw} ->
			WS ! {game_over, draw},
			Human_bot_games1 = lists:delete(Game,Human_bot_games),
			game_manager(Schedule,Human_bot_games1,Bot_bot_gameNBR,Won,Draw+1,Lost,GamesDone+1);

		{game_over,WS,Game,Last_move,man_lost} ->
			WS ! {game_over, Last_move, man_lost},
			Human_bot_games1 = lists:delete(Game,Human_bot_games),
			game_manager(Schedule,Human_bot_games1,Bot_bot_gameNBR,Won+1,Draw,Lost,GamesDone+1);
			
		{game_over,WS,Game,Last_move,draw} ->
			WS ! {game_over, Game, Last_move, draw},
			Human_bot_games1 = lists:delete(Game,Human_bot_games),
			game_manager(Schedule,Human_bot_games1,Bot_bot_gameNBR,Won,Draw+1,Lost,GamesDone+1);
		
		bot_game_over ->
			Size = gyri:brain_size(),
			if
				Size > 1000000 -> io:format("Brain size:~p,000,000~n",[Size div 1000000]);
				Size > 1000 -> io:format("Brain size:~p,000~n",[ Size div 1000]);
				true -> io:format("Brain size:~p,000~n",[Size])
			end,
			game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR-1,Won,Draw,Lost,GamesDone+1);

		%% TODO: delete this
		Err -> throw(Err)
		
 	after
 		1000 ->
 			if Bot_bot_gameNBR < ?BOT_BOT_GAMES_LIMIT ->
 				GS = self(),
 				spawn(?MODULE,start_new_bot_game,[Schedule,GS]),
 				game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR+1,Won,Draw,Lost,GamesDone);
 			true ->
 				game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone)
 			end
	end.



start_new_bot_game(Schedule,GS) ->
	State = state:init_state(),
	run_bot_game(Schedule,GS,hard,none,none,none,none,State).

run_bot_game(Schedule,GS,Level,MyPrevState,MyPrevMove,OppPrevState,OppPrevMove,State) ->
	Move = bot:get_move(Level,MyPrevState,MyPrevMove,OppPrevState,OppPrevMove,State),
	%io:format("Bot move:~p, State:~p~n",[Move,State]),
	%Move = rand:rand(State),
	case change_state(State,Move) of
		blacks_won -> GS ! bot_game_over;
		whites_won -> GS ! bot_game_over;
		draw -> GS ! bot_game_over;
		NextState -> run_bot_game(Schedule,GS,Level,OppPrevState,OppPrevMove,State,Move,NextState)
	end.




start_new_game(Schedule,GS,Level,blacks,WS) -> %% run Agent vs. Bot
	% bot plays for blacks
	State = state:init_state(),
	run_game(Schedule,GS,Level,none,none,none,none,State,blacks,WS);
start_new_game(Schedule,GS,Level,whites,WS) -> %% run Agent vs. Bot
	% bot plays for whites
	State = state:init_state1(),
	OppPrevState = state:init_state(),
	OppPrevMove = {8,8},
	run_game(Schedule,GS,Level,none,none,OppPrevState,OppPrevMove,State,whites,WS).


run_game(Schedule,GS,Level,MyPrevState,MyPrevMove,OppPrevState,OppPrevMove,{Turn,_Board}=State,Color,WS) ->
	case color(Turn) =:= Color of
		true -> % your move
			Move = bot:get_move(Level,MyPrevState,MyPrevMove,OppPrevState,OppPrevMove,State),
			%io:format("Bot move:~p, State:~p~n",[Move,State]),
			%Move = rand:rand(State),
			case change_state(State,Move) of
				blacks_won -> GS ! {game_over, WS, self(), Move, man_lost}, gyri:save_gyri();
				whites_won -> GS ! {game_over, WS, self(), Move, man_lost}, gyri:save_gyri();
				draw -> GS ! {game_over, WS, self(), Move, draw}, gyri:save_gyri();
				NextState -> GS ! {bot_move, WS, self(), Move},
					run_game(Schedule,GS,Level,OppPrevState,OppPrevMove,State,Move,NextState,Color,WS)
			end;
		false->  % Opponent's move
			receive
				quit -> ok;		
				{move, Move} ->
					%io:format("Human move:~p, State:~p~n",[Move,State]),
					case change_state(State,Move) of
						blacks_won -> GS ! {game_over, WS, self(), man_won}, gyri:save_gyri();
						whites_won -> GS ! {game_over, WS, self(), man_won}, gyri:save_gyri();
						draw -> GS ! {game_over, WS, self(), draw}, gyri:save_gyri();
						NextState -> run_game(Schedule,GS,Level,OppPrevState,OppPrevMove,State,Move,NextState,Color,WS)
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
			Next_state = illegal_state
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
