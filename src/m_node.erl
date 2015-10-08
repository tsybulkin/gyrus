%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(m_node).
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
			io:format("Schedule: ~p~n",[Schedule]),
			lists:foreach( fun({Pid,Workload})-> Pid ! {workload,Workload} end,Schedule),

			WS = start_web_server(),

			game:game_manager(Schedule,WS),

			shutdown(Acc)
	end.



start_web_server() -> ok.



schedule([]) -> error(no_nodes);
schedule(Pids) ->
	Data_dir = "data",
	
	io:format("Got ~p girus nodes~n",[length(Pids)]),
	{ok,Ls} = file:list_dir(Data_dir),
	Files = lists:foldl( fun(File, Acc) -> 
							Size = filelib:file_size(Data_dir++"/"++File),
							[{File,Size}|Acc]							
						end, [], Ls),
	Mem_per_node = lists:sum([ S || {_,S} <- Files]) / length(Pids),
	distribute(Pids,Files,Mem_per_node).



distribute([Pid],Files,_Mem_per_node) ->	[{Pid,Files}];
distribute([Pid|Pids],Files,Mem_per_node) -> distribute(Pids,Files,Mem_per_node,[{Pid,[]}],0).
	
distribute(Pids,[{File,Size}|Files],Mem_per_node,[{Pid,Acc}|Schedule],CumSize) when CumSize<Mem_per_node ->
	distribute(Pids,Files,Mem_per_node,[{Pid,[{File,Size}|Acc]}|Schedule],CumSize+Size);
distribute([Pid],Files,_Mem_per_node,Schedule,_CumSize) ->
	[{Pid,Files}|Schedule];
distribute([Pid|Pids],Files,Mem_per_node,Schedule,CumSize) ->
	distribute(Pids,Files,Mem_per_node,[{Pid,[]}|Schedule],CumSize-Mem_per_node).



shutdown(Gyrus_pids) -> lists:foreach(fun(Pid)-> Pid ! quit end, Gyrus_pids).


