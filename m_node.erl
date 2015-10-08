%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(cluster).
-export([start/0]).

-define(GYRUS_CONNECT_TIMEOUT, 3000).

% First, Erlang nodes for gyrus should be started. Then start() at main node is started.
% start() at gyrus is trying to ping 'main' node. When successful it send a message
% with its pid and is waiting for workload
% 

start() -> 
	register(main,self()),
	start([]).

start(Acc) ->
	receive
		{ready, Pid} -> 
			io:format("Got pid: ~p~n",[Pid]),
			Pid ! connected,
			start([Pid|Acc])
	after 
		?GYRUS_CONNECT_TIMEOUT ->
			Schedule = schedule(Acc),
			io:format("Shedule: ~p~n",[Schedule]),

			shutdown(Acc)
	end.


schedule(Pids) ->
	io:format("Got ~p girus nodes~n",[length(Pids)]),
	lists:foreach( fun(Pid)-> Pid ! workload end,Pids).
	


shutdown(Gyrus_pids) -> lists:foreach(fun(Pid)-> Pid ! quit end, Gyrus_pids).


