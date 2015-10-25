%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(bot).
-export([get_move/6, gyrus_name/1
		]).

-define(NBR_EPISODES,100).


get_move(_Level,_,_,_,_,{1,_Board}) -> {8,8};
get_move(Level,MyPrevState,MyPrevMove,OppPrevState,OppPrevMove,{Turn,_}=State) ->
	%state:print_board(Board),
	%gyri:check_gyrus(Turn),
	%io:format("~n * * Turn:~p ~n",[Turn]),
	case get_best_worst_state_moves(State) of
		no_policy -> %io:format("no_policy ~n"),
			Moves = moves:get_selected_moves(State),
			N = episodes_nbr(Level),
			%io:format("doing simulations for moves: ~p~n",[Moves]),
			get_best_simulation(OppPrevState,OppPrevMove,Moves,State,N);
			
		{worst_moves,Worst_moves} -> 
			%io:format("Worst moves for state:~p~n~p~n",[State,Worst_moves]),
			Moves = moves:get_selected_moves(State),
			case lists:filter(fun(M)-> not lists:member(M,Worst_moves) end, Moves) of
				[] -> %io:format("NO GOOD MOVES~n"),
					%io:format("Opponent's previous move: ~p was the best~n",[OppPrevMove]),
					%io:format("My previous move: ~p was the worst~n",[MyPrevMove]),
					learn(OppPrevState,OppPrevMove,max_value(game:color(Turn-1))),
					learn(MyPrevState,MyPrevMove,min_value(game:color(Turn-2))),
					io:format("  Turn:~p Losing moves:~p~n",[Turn,Moves]),
					rand:pick_randomly(Moves);
				Good_moves -> 
					N = episodes_nbr(Level),
					Move = get_best_simulation(OppPrevState,OppPrevMove,Good_moves,State,N),
					io:format("    Losing moves:~p~n",[Worst_moves]),
					Move
			end;

		{best_moves,Best_moves} -> 
			%io:format("Best moves:~p~n for",[Best_moves]), state:print_state(State),
			learn(OppPrevState,OppPrevMove,min_value(game:color(Turn-1))),
			io:format("  Turn:~p Winning moves:~p~n",[Turn,Best_moves]),
			rand:pick_randomly(Best_moves)
	end.



get_best_simulation(PrevState,PrevMove,Moves,{Turn,_}=State,N) ->
	Scores = monte_carlo(PrevState,PrevMove,State,Moves, N div length(Moves)),	
	%io:format("~nScores: ~p~n",[Scores]),

	Best_moves = lists:sublist([XY || {_,XY}<-Scores],3),
	N_per_move = N div length(Best_moves),
	Refined = monte_carlo(PrevState,PrevMove,State,Best_moves, N_per_move),	
	io:format("Turn:~p  Refined moves: ~p~n",[Turn,Refined]),

	[{_,Move}|_] = Refined,
	%if abs(Score) =:= N_per_move -> 
	%	learn(State,Move,Score div N_per_move),
	%	learn(PrevState,PrevMove,Score div N_per_move); 
	%	true -> ok 
	%end,
	Move.



monte_carlo(PrevState,PrevMove,{Turn,_}=State,Moves,Simulation_Nbr) ->
	Depth = 1+round(2 * math:log(1+Simulation_Nbr) ),
	%io:format("Monte carlo over: ~p~n",[Moves]),
	lists:sort( fun({A,_},{B,_})-> A>B end,lists:foldl(
		fun(Move,Acc) ->
			CumScore = 
			lists:foldl(fun(_,ScoreAcc)-> run_episode(PrevState,PrevMove,State,Depth,Move)+ScoreAcc
						end,0,lists:seq(1,Simulation_Nbr)),
			case game:color(Turn) of
				blacks -> [{-CumScore,Move}|Acc];
				whites -> [{CumScore,Move}|Acc]
			end
			
		end, [], Moves)).



run_episode(PrevState,PrevMove,{Turn,_}=State,Depth,Move) ->
	case game:change_state(State,Move) of
		blacks_won -> learn(State,Move,-1),learn(PrevState,PrevMove,-1),-1;
		whites_won -> learn(State,Move, 1),learn(PrevState,PrevMove, 1), 1;
		draw -> 0;
		Next_state -> 
			%gyri:check_gyrus(Turn+1),
			{NextMove,NextStateValue} = get_policy_value(Next_state),
			%state:print_state(State),
			%io:format("Move:~p Value:~p, Next_move:~p~n",[Move,NextStateValue,NextMove]),
			%state:print_state(Next_state),
			learn(State,Move,NextStateValue),
			case {game:color(Turn),NextStateValue} of
				{blacks,-1} -> learn(PrevState,PrevMove,NextStateValue);
				{whites, 1} -> learn(PrevState,PrevMove,NextStateValue);
				_ -> do_nothing
			end,
			case {Depth,NextStateValue} of
				{1,0} -> 0;
				{_,0} -> run_episode(State,Move,Next_state,Depth-1,NextMove);
				{_,V} -> V
			end
	end.




%% ets table must be created as [named_table,bag]
learn(_,_,0) -> ok;
learn(none,none,-1) -> ok;
learn({1,_},Move,Val) -> io:format("~ngame solved for the move: ~p!~nblacks won: ~p~n",[Move,Val]);
learn({Turn,Board},{X,Y},Next_state_value) ->
	{X0,Y0,Position} = state:board_to_position(Board),
	X1=X-X0, Y1=Y-Y0,
	Key = state:get_key(Position),	
	Gyrus = gyrus_name(Turn),
	%io:format("learning at state:~p(~p) that move:~p leads to ~p~n",[game:color(Turn),Turn,{X,Y},Next_state_value]),

	case {game:color(Turn),Next_state_value} of
		{blacks,1} -> save_worst_move(X1,Y1,Position,Key,Gyrus);
		{blacks,-1} -> save_best_move(X1,Y1,Position,Key,Gyrus);
		{whites,1} -> save_best_move(X1,Y1,Position,Key,Gyrus);
		{whites,-1} -> save_worst_move(X1,Y1,Position,Key,Gyrus)
	end.



save_best_move(X,Y,Position,Key,Gyrus) ->
	%io:format("Saving best move: ~p at position ~p~n",[{X,Y},Position]),
	case ets:lookup(Gyrus,Key) of
		[] -> ets:insert(Gyrus,{Key,Position,[{X,Y}],[]});
		Values -> 
			case get_variant_values(Position,1,Values) of
				not_found -> ets:insert(Gyrus,{Key,Position,[{X,Y}],[]});
				{Var,SymPosition,Best_moves,Worst_moves} ->
					%io:format("Found: ~p~n",[{Var,SymPosition,Best_moves,Worst_moves}]),
					{X1,Y1} = state:transform(X,Y,Var,Position),
					case lists:member({X1,Y1},Best_moves) of
						true -> ok;
						false->
							Values1 = lists:keyreplace(SymPosition,2,Values,{Key,SymPosition,[{X1,Y1}|Best_moves],Worst_moves}),
							%io:format("Old values:~p~nNew Values: ~p~n",[Values,Values1]),
							ets:delete(Gyrus,Key),
							ets:insert(Gyrus,Values1)
					end
			end
	end.
	%io:format("~p: ~p~n",[Gyrus,ets:lookup(Gyrus,Key)]).



save_worst_move(X,Y,Position,Key,Gyrus) ->
	%io:format("Saving worst move: ~p at position ~p~n",[{X,Y},Position]),
	case ets:lookup(Gyrus,Key) of
		[] -> ets:insert(Gyrus,{Key,Position,[],[{X,Y}]});
		Values -> 
			case get_variant_values(Position,1,Values) of
				not_found -> ets:insert(Gyrus,{Key,Position,[],[{X,Y}]});
				{Var,SymPosition,Best_moves,Worst_moves} ->
					%io:format("Found: ~p~n",[{Var,SymPosition,Best_moves,Worst_moves}]),
					{X1,Y1} = state:transform(X,Y,Var,Position),
					case lists:member({X1,Y1},Worst_moves) of
						true -> ok;
						false->
							Values1 = lists:keyreplace(SymPosition,2,Values,{Key,SymPosition,Best_moves,[{X1,Y1}|Worst_moves]}),
							ets:delete(Gyrus,Key),
							%io:format("Old values:~p~nNew Values: ~p~n",[Values,Values1]),
							ets:insert(Gyrus,Values1)
					end
			end
	end.
	%io:format("~p: ~p~n",[Gyrus,ets:lookup(Gyrus,Key)]).



get_policy_value({Turn,_}=State) ->
	case get_best_worst_state_moves(State) of
		no_policy -> %io:format("no_policy for state:~p~n",[State]),
			{rand:pick_randomly(moves:get_selected_moves(State)),0};
		{worst_moves,Worst_moves} -> 
			%io:format("Worst moves for state:~p~n~p~n",[State,Worst_moves]),
			Moves = moves:get_selected_moves(State),
			case lists:filter(fun(M)-> not lists:member(M,Worst_moves) end, Moves) of
				[] -> {rand:pick_randomly(Moves),min_value(game:color(Turn))};
				Good_moves -> {rand:pick_randomly(Good_moves),0}
			end;
		{best_moves,Best_moves} -> 
			%io:format("Best moves for state:~p~n~p~n",[State,Best_moves]),
			{rand:pick_randomly(Best_moves),max_value(game:color(Turn))}
	end.



get_best_worst_state_moves({Turn,Board}) ->
	{X0,Y0,Position} = state:board_to_position(Board),
	Key = state:get_key(Position),
	Gyrus = gyrus_name(Turn),
	case ets:lookup(Gyrus,Key) of
		[] ->
			%io:format("no Key found for ~p~n",[{X0,Y0,Position}]), 
			no_policy;
		Values -> 
			case match_position(Position,1,Values) of
				no_match -> no_policy;
				{Type,Moves,Variant} -> 
					%if Variant=/=1 -> 
					%	io:format("Match found: ~p~n",[{Type,Moves,Variant}]),
					%	state:print_board(Board);
					%	true->ok 
					%end,
					case state:filter_legal(Moves,X0,Y0,Variant,Position,{Turn,Board}) of 
						[] -> no_policy;
						Filtered -> {Type,Filtered}
					end
			end
	end.
	


match_position(Position,Variant,Values) ->
	case lists:keyfind(Position,2,Values) of
		false -> 
			case Variant of
				-1 -> no_match;
				_  ->
					{Position1,Next_var} = state:next_variant(Position,Variant),
					match_position(Position1,Next_var,Values)
			end;

		{_,Position,[],Worst} -> 
			%if Variant=/=1 -> 
			%	io:format("Position: ~p~n",[Position]); 
			%	true->ok 
			%end, 
			{worst_moves,Worst,Variant};
		{_,Position,Best,_} -> 
			%if Variant=/=1 -> 
			%			io:format("Position: ~p~n",[Position]); 
			%			true->ok 
			%end,
			{best_moves,Best,Variant}
	end.



get_variant_values(_Position,-1,_Values) -> not_found;
get_variant_values(Position,Variant,Values) ->
	case lists:keyfind(Position,2,Values) of
		false -> 
			{Position1,Next_var} = state:next_variant(Position,Variant),
			get_variant_values(Position1,Next_var,Values);

		{_,Position,Best_moves,Worst_moves} -> {Variant,Position,Best_moves,Worst_moves}
	end.




gyrus_name(J) -> list_to_atom("gyrus"++integer_to_list(J)).



min_value(blacks) -> 1;
min_value(whites) -> -1.

max_value(blacks) -> -1;
max_value(whites) -> 1.



episodes_nbr(easy)  -> 21;
episodes_nbr(medium)-> ?NBR_EPISODES;
episodes_nbr(hard)  -> ?NBR_EPISODES*5.

