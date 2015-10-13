%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(bot).
-export([get_move/2]).



get_move(State,Level) ->
	case moves:get_selected_moves(State) of
		[Move] ->
			%io:format("One move selected~n"), 
			Move;
		[_|_]=Moves ->
			Scores = monte_carlo(State,Moves,?NBR_EPISODES div length(Moves)),	
			% Scores = eflame:apply(?MODULE,monte_carlo,[State,Moves,?NBR_EPISODES div length(Moves)]),	
			io:format("~nScores: ~p~n",[Scores]),

			Best_moves = lists:sublist([XY || {_,XY}<-Scores],3),
			Refined = monte_carlo(State,Best_moves,90 div length(Best_moves)),	
			io:format("~nRefined: ~p~n",[Refined]),

			get_softmax_policy(State,[ XY || {_,XY}<-Refined],true)
	end.



