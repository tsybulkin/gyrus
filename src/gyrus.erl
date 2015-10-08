%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gyrus).
-export([gyrus/2]).



gyrus(File,Main_node) ->
	{main,Main_node} ! {File,self()},
	case ets:file2tab(File) of
		{ok,_} -> ok;
		{error,Reason} ->
			io:format("Error occured. Reason: ~p~nCreating new~n",[Reason]),
			ets:new(list_to_atom(File),[named_table])
	end,
	io:format("gyrus done~n"),

	receive
		quit -> io:format("Girus: quiting...~n");

		{next_gyrus,Next_gyrus_pid} -> gyrus(File,Main_node,Next_gyrus_pid)
	end.


gyrus(_Gyrus,_Main_node,_Next_gyrus_Pid) ->
	receive
		quit -> io:format("Girus: quiting...~n");

		{get_state_value, _Game, _State, Pid} -> Pid ! 0;

		{get_move, _Game, _State, Pid} -> Pid ! {8,8}
	end.

