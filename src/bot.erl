%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(bot).
-export([get_move/3]).

-define(NBR_EPISODES,600).


get_move(Gyri,Level,State) ->
	case moves:get_selected_moves(State) of
		[Move] ->
			%io:format("One move selected~n"), 
			Move;
		[_|_]=Moves ->
			Scores = monte_carlo(State,Moves,episodes_nbr(Level) div length(Moves)),	
			% Scores = eflame:apply(?MODULE,monte_carlo,[State,Moves,?NBR_EPISODES div length(Moves)]),	
			io:format("~nScores: ~p~n",[Scores]),

			Best_moves = lists:sublist([XY || {_,XY}<-Scores],3),
			Refined = monte_carlo(State,Best_moves,90 div length(Best_moves)),	
			io:format("~nRefined: ~p~n",[Refined]),

			get_softmax_policy(State,[ XY || {_,XY}<-Refined],true)
	end.



monte_carlo({Turn,_}=State,Moves,Simulation_Nbr) ->
	Depth = 2*round(math:log(Simulation_Nbr)),
	lists:sort( fun({A,_},{B,_})-> A>B end,lists:foldl(
		fun(Move,Acc) ->
			CumScore = 
			lists:foldl(fun(_,ScoreAcc)-> run_episode(State,Depth,Move)+ScoreAcc
						end,0,lists:seq(1,Simulation_Nbr)),
			case game:color(Turn) of
				blacks -> [{-CumScore,Move}|Acc];
				whites -> [{CumScore,Move}|Acc]
			end
			
		end, [], Moves)).



run_episode(_State,0,_Move) -> 0;
run_episode(State,Depth,Move) ->
	case game:change_state(State,Move) of
		blacks_won -> learn(State,Move,blacks_won), -1;
		whites_won -> learn(State,Move,whites_won), 1;
		draw -> 0;
		Next_state -> 
			learn(State,Move,Next_state),
			run_episode(Next_state,Depth-1,get_policy(Next_state,false))
	end.


%% ets table must be created as [named_table,bag]

learn({Turn,Board}=State,Move,{_,Board1}=Next_state) ->
	Position = state:board_to_position(Board),
	Key = state:get_key(Position),
	
	Gyrus1 = list_to_atom("gyr"++integer_to_list(Turn)),
	Gyrus2 = list_to_atom("gyr"++integer_to_list(Turn+1)).




get_policy(State,Print) ->
	Moves = moves:get_selected_moves(State),
	case get_state_value_action(State,Moves,Print) of
		{0,_} -> get_softmax_policy(State, Moves,Print);
		{_,Move} -> Move
	end.




get_softmax_policy(State,[M|Moves],Print) -> get_softmax_policy(State,[M|Moves],Print,M).
get_softmax_policy(State,[M|Moves],Print,FMove) ->
	case rand:flip_coin(0.8) of
		false -> M;
		_ -> get_softmax_policy(State,Moves,Print,FMove)
	end;
get_softmax_policy(_,[],_,FMove) -> FMove.




episodes_nbr(easy)  -> ?NBR_EPISODES;
episodes_nbr(medium)-> ?NBR_EPISODES*3;
episodes_nbr(hard)  -> ?NBR_EPISODES*10.

