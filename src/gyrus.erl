%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gyrus).
-export([gyrus/2]).



gyrus(Gyrus,Main_node) ->
	{main,Main_node} ! {Gyrus,self()},

	receive
		quit -> io:format("Girus: quiting...~n");

		{next_gyrus,Next_gyrus_pid} -> gyrus(Gyrus,Main_node,Next_gyrus_pid)
	end.


gyrus(Gyrus,Main_node,Next_gyrus_Pid) ->
	receive
		quit -> io:format("Girus: quiting...~n");

		{get_state_value, Game, State, Pid} -> Pid ! 0;

		{get_move, Game, State, Pid} -> Pid ! {8,8}
	end.

