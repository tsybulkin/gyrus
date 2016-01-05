%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(game).
-export([start_link/0]).
-export([game_manager/7,
    	game_manager_call/1,
		start_new_game/5, start_new_bot_game/2, start_new_demo_game/2,
		% change_state/2,
		color/1
		]).

-define(HUMAN_BOT_GAMES_LIMIT, 50).
-define(BOT_BOT_GAMES_LIMIT, 1).

start_link() ->
	Schedule = [],
	Pid = spawn_link(?MODULE, game_manager, [Schedule,[],1,0,0,0,0]),
	DemoPid = spawn(?MODULE,start_new_demo_game,[Schedule,Pid]),
        true = register(demo_game, DemoPid),
	gyri:init_gyri(),
	true = register(game_manager, Pid),
	{ok, Pid}.

game_manager_call(Request) ->
	game_manager ! Request,
	receive Response -> Response end.


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

		{game_over,WS,Game,Fiver,man_won} ->
			WS ! {game_over,Fiver,man_won},
			WS ! {counters, Won,Draw,Lost+1,GamesDone+1},
			Human_bot_games1 = lists:delete(Game,Human_bot_games),
			game_manager(Schedule,Human_bot_games1,Bot_bot_gameNBR,Won,Draw,Lost+1,GamesDone+1);
		
		{game_over,WS,Game,draw} ->
			WS ! {game_over, draw},
			WS ! {counters,Won,Draw+1,Lost,GamesDone+1},
			Human_bot_games1 = lists:delete(Game,Human_bot_games),
			game_manager(Schedule,Human_bot_games1,Bot_bot_gameNBR,Won,Draw+1,Lost,GamesDone+1);

		{game_over,WS,Game,Fiver,man_lost} ->
			WS ! {game_over, Fiver, man_lost},
			WS ! {counters, Won+1,Draw,Lost,GamesDone+1},
			Human_bot_games1 = lists:delete(Game,Human_bot_games),
			game_manager(Schedule,Human_bot_games1,Bot_bot_gameNBR,Won+1,Draw,Lost,GamesDone+1);
			
		bot_game_over ->
			Size = gyri:brain_size(),
			if
				Size > 1000000 -> io:format("Brain size:~p,000,000~n",[Size div 1000000]);
				Size > 1000 -> io:format("Brain size:~p,000~n",[ Size div 1000]);
				true -> io:format("Brain size:~p~n",[Size])
			end,
			game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR-1,Won,Draw,Lost,GamesDone+1);


		%% DEMO game showing in webpage
		{new_visitor,WS} -> % add_visitor_to_subscription_list,
			game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone);

		new_demo_game -> % start_new_demo_game,
			io:format("New DEMO game started~n"),
			game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone);

		{demo_game_state,State} -> % send_current_State,
			game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone);

		{demo_game_over,Pids} ->
			distribute({counters, Won,Draw,Lost,GamesDone+1}, Pids),
			game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone+1);

		{demo_game_move,Color,Move} -> % send_new_move,
			game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone);

                {get_counters, Pid} ->
			Pid ! {counters, Won,Draw,Lost,GamesDone},
                        game_manager(Schedule,Human_bot_games,Bot_bot_gameNBR,Won,Draw,Lost,GamesDone);

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
	%Level = rand:pick_randomly([hard,medium,hard]),
	run_bot_game(Schedule,GS,hard,none,none,none,none,State).

run_bot_game(Schedule,GS,Level,MyPrevState,MyPrevMove,OppPrevState,OppPrevMove,State) ->
	Move = bot:get_move(Level,MyPrevState,MyPrevMove,OppPrevState,OppPrevMove,State),
	%io:format("Bot move:~p, State:~p~n",[Move,State]),
	%Move = rand:rand(State),
	case state:change_state(State,Move) of
		{blacks_won,_Fiver} -> GS ! bot_game_over;
		{whites_won,_Fiver} -> GS ! bot_game_over;
		draw -> GS ! bot_game_over;
		NextState -> run_bot_game(Schedule,GS,Level,OppPrevState,OppPrevMove,State,Move,NextState)
	end.

start_new_demo_game(Schedule,GS) ->
  start_new_demo_game(Schedule,GS,sets:new()).

start_new_demo_game(Schedule,GS,Subscribers) ->
	State = state:init_state(),
	timer:sleep(3000),
	GS ! new_demo_game,
	run_demo_game(Schedule,GS,medium,none,none,none,none,State,Subscribers).

distribute(Message, Pids) ->
  true = sets:is_set(Pids),
  lists:foreach(fun(Pid) ->
    Pid ! Message
  end, sets:to_list(Pids)).

run_demo_game(Schedule,GS,Level,MyPrevState,MyPrevMove,OppPrevState,OppPrevMove,{Turn,Board,_}=State,Pids) ->
	%io:format("Demo game next move~n"),
	Pids1 = receive
      {subscribe, Pid} ->
       	Board1 = lists:map(fun tuple_to_list/1, tuple_to_list(Board)),
        Pid ! {demo_game_board, Board1},
        sets:add_element(Pid, Pids);
      {unsubscribe, Pid} ->
        sets:del_element(Pid, Pids)
    after 0 ->
      Pids
    end,

	Move = bot:get_move(Level,MyPrevState,MyPrevMove,OppPrevState,OppPrevMove,State),
	%io:format("Bot move:~p~n",[Move]), state:print_board(Board),
	%Move = rand:rand(State),

	case state:change_state(State,Move) of
		{blacks_won,Fiver} ->
			game_manager ! {demo_game_over, Pids1},
			distribute({demo_game_over,blacks,Fiver}, Pids1), start_new_demo_game(Schedule,GS,Pids1);
		{whites_won,Fiver} -> 
			game_manager ! {demo_game_over,Pids1},
			distribute({demo_game_over,whites,Fiver}, Pids1), start_new_demo_game(Schedule,GS,Pids1);
		draw -> 
			game_manager ! {demo_game_over,Pids1},
            distribute({demo_game_over,draw}, Pids1), start_new_demo_game(Schedule,GS,Pids1);
		NextState -> 
			distribute({demo_game_move,color(Turn),Move}, Pids1),
			run_demo_game(Schedule,GS,Level,OppPrevState,OppPrevMove,State,Move,NextState, Pids1)
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
			case state:change_state(State,Move) of
				{Won,Fiver} when blacks_won==Won; whites_won==Won -> GS ! {game_over, WS, self(), Fiver, man_lost}, gyri:save_gyri();
				draw -> GS ! {game_over, WS, self(), draw}, gyri:save_gyri();
				NextState -> GS ! {bot_move, WS, self(), Move},
					run_game(Schedule,GS,Level,OppPrevState,OppPrevMove,State,Move,NextState,Color,WS)
			end;
		false->  % Opponent's move
			receive
				quit -> ok;		
				{move, Move} ->
					%io:format("Human move:~p, State:~p~n",[Move,State]),
					case state:change_state(State,Move) of
						{Won,Fiver} when blacks_won==Won; whites_won==Won -> GS ! {game_over, WS, self(), Fiver,man_won}, gyri:save_gyri();
						draw -> GS ! {game_over, WS, self(), draw}, gyri:save_gyri();
						NextState -> run_game(Schedule,GS,Level,OppPrevState,OppPrevMove,State,Move,NextState,Color,WS)
					end
			end
	end.




color(Turn) -> if (Turn rem 2) =:= 0 -> whites; true -> blacks end.
