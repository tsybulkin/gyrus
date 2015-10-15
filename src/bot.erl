%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(bot).
-export([get_move/3, gyrus_name/1
		]).

-define(NBR_EPISODES,600).


get_move(MaxGyrus,Level,State) ->
	case moves:get_selected_moves(State) of
		[Move] ->
			%io:format("One move selected~n"), 
			Move;
		[_|_]=Moves ->
			N = episodes_nbr(Level),
			Scores = monte_carlo(MaxGyrus,State,Moves, N div length(Moves)),	
			% Scores = eflame:apply(?MODULE,monte_carlo,[State,Moves,?NBR_EPISODES div length(Moves)]),	
			io:format("~nScores: ~p~n",[Scores]),

			Best_moves = lists:sublist([XY || {_,XY}<-Scores],3),
			Refined = monte_carlo(MaxGyrus,State,Best_moves, N div length(Best_moves)),	
			io:format("~nRefined: ~p~n",[Refined]),

			[{_,Move}|_] = Refined, Move
	end.



monte_carlo(MaxGyrus,{Turn,_}=State,Moves,Simulation_Nbr) ->
	Depth = 2*round(math:log(Simulation_Nbr)),
	lists:sort( fun({A,_},{B,_})-> A>B end,lists:foldl(
		fun(Move,Acc) ->
			CumScore = 
			lists:foldl(fun(_,ScoreAcc)-> run_episode(MaxGyrus,State,Depth,Move)+ScoreAcc
						end,0,lists:seq(1,Simulation_Nbr)),
			case game:color(Turn) of
				blacks -> [{-CumScore,Move}|Acc];
				whites -> [{CumScore,Move}|Acc]
			end
			
		end, [], Moves)).



run_episode(_MaxGyrus,_State,0,_Move) -> 0;
run_episode(MaxGyrus,{Turn,_}=State,Depth,Move) ->
	case game:change_state(State,Move) of
		blacks_won -> learn(State,Move,-1),-1;
		whites_won -> learn(State,Move, 1), 1;
		draw -> 0;
		Next_state -> 
			if Turn =:= MaxGyrus -> gyri:new_gyrus(Turn+1); true -> ok end,
			Gyrus2 = gyrus_name(Turn+1),
			case get_state_value(Next_state) of
				0 -> ok;
				V -> learn(State,Move,V)
			end,
			run_episode(MaxGyrus+1,Next_state,Depth-1,get_policy(Next_state))
	end.



get_state_value(State) -> 0.
% NOT FINISHED


%% ets table must be created as [named_table,bag]
learn({Turn,Board}=State,Move,Next_state_value) ->
	{_,_,Position} = state:board_to_position(Board),
	Key = state:get_key(Position),	
	Gyrus1 = gyrus_name(Turn),
	io:format("Position: ~p~nMove: ~p  Value: ~p~n",[Position,Move,Next_state_value]).
	%% NOT FINISHED



get_policy(State) ->
	case get_best_worst_state_moves(State) of
		no_policy -> moves:get_selected_moves(State);
		{worst_moves,Worst_moves} -> 
			Moves = moves:get_selected_moves(State),
			case lists:filter(fun(M)-> lists:member(M,Worst_moves) end, Moves) of
				[] -> io:format("NO GOOD MOVES~n"), rand:pick_randomly(Moves);
				Good_moves -> rand:pick_randomly(Good_moves)
			end;
		{best_moves,Best_moves} -> rand:pick_randomly(Best_moves)
	end.



get_best_worst_state_moves({Turn,Board}) ->
	{X0,Y0,Position} = state:board_to_position(Board),
	Key = state:get_key(Position),
	Gyrus = gyrus_name(Turn),
	case ets:lookup(Gyrus,Key) of
		[] -> no_policy;
		Values -> 
			case match_position(Position,1,Values) of
				no_match -> no_policy;
				{Type,Moves,Variant} -> 
					case state:filter_legal(Moves,X0,Y0,Variant,Position) of 
						[] -> no_policy;
						Filtered -> {Type,Filtered}
					end
			end
	end.
	


match_position(_Position,-1,_Values) -> no_match;
match_position(Position,Variant,Values) ->
	case lists:keyfind(Position,1,Values) of
		false -> 
			{Position1,Next_var} = state:next_variant(Position,Variant),
			match_position(Position1,Next_var,Values);

		{_,[],Worst} -> {worst_moves,Worst,Variant};
		{_,Best,_} -> {best_moves,Best,Variant}
	end.



gyrus_name(J) -> list_to_atom("gyrus"++integer_to_list(J)).



episodes_nbr(easy)  -> ?NBR_EPISODES;
episodes_nbr(medium)-> ?NBR_EPISODES*3;
episodes_nbr(hard)  -> ?NBR_EPISODES*10.

