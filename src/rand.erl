%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(rand).
-export([rand/1,
		pick_randomly/1,
		get_random_move/2,
		flip_coin/1
		]).
-define(THR,0.75).



rand({Turn,Board}) ->
	Lines = lines:extract_lines(Board),
	case game:color(Turn) of
		whites -> Own = w, Opp = b;
		blacks -> Own = b, Opp = w
	end,

	case moves:get_mat(Own,Opp,Lines) of
		not_found ->
			case moves:get_enforced_moves(Own,Opp,Lines) of
				[] ->
					%io:format("No enforced move found~n"),
					case moves:get_good_moves(Own,Opp,Lines) of
						[] ->
							get_random_move(Board,1);
						Ms ->
							pick_randomly(Ms)
					end;
				Ms ->
					io:format("~p enforced moves found:~n~p~n",[length(Ms),Ms]),
					pick_randomly(Ms)
			end;
		{X,Y} -> {X,Y}
	end.


get_random_move(_,15) -> no_action;
get_random_move(Board,N) ->
	I = 7 + random:uniform(N) - N div 2,
	J = 7 + random:uniform(N) - N div 2,
	case element(I,element(J,Board)) of
		e -> {I,J};
		_ -> get_random_move(Board,N+2)
	end.


flip_coin(P) -> P < random:uniform().



pick_randomly(Moves) ->
	lists:nth(random:uniform(length(Moves)),Moves).
