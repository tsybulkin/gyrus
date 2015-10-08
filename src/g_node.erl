%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(g_node).
-export([start/0]).

-define(PING_FREQUENCY,1000).


start() ->
	Main_node = 'main@Ians-MacBook-Air.local',
	pinging(Main_node).

pinging(Main_node) ->
	case net_adm:ping(Main_node) of
		pang -> timer:sleep(?PING_FREQUENCY), pinging(Main_node);
		pong -> io:format("ping-pong~n"), connect_main(Main_node)
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
		{workload,Workload} -> 
			io:format("Girus: Got workload:~p~n",[Workload]),
			lists:foreach(  fun({Name,_Size})-> 
								spawn(gyrus,gyrus,[Name, Main_node])
							end,Workload)
	end.



