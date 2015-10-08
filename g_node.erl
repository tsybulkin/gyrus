%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gyrus).
-export([start/0]).

-define(PING_FREQUENCY,1000).


start() ->
	Main_node = 'main@Ians-MacBook-Air.local',
	ping(Main_node).

ping(Main_node) ->
	case net_adm:ping(Main_node) of
		pang -> timer:sleep(?PING_FREQUENCY), ping(Main_node);
		pong -> connect_main(Main_node)
	end.


connect_main(Main_node) ->
	{main,Main_node} ! {ready, self()},
	receive
		connected -> io:format("Connected~n"), get_workload(Main_node)
	after ?PING_FREQUENCY ->
		%io:format("trying to connect ...~n"),
		connect_main(Main_node)
	end.



get_workload(Main_node) ->
	receive
		workload -> 
			io:format("Girus: Got workload~n"),
			girus(Main_node)
	end.



girus(Main_node) ->
	receive
		quit -> io:format("Girus: quiting...~n");
		Msg ->
			io:format("Girus: got message: ~p~n",[Msg]),
			girus(Main_node)
	end.


