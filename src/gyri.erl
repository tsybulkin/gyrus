%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gyri).
-export([init_gyri/0, save_gyri/0, new_gyrus/1, check_gyrus/1
		]).


init_gyri() ->
	{ok,Files} = file:list_dir("data"),
	lists:foreach(  fun(J)-> 
						Gyrus = "gyrus"++integer_to_list(J),
						case lists:member(Gyrus,Files) of
							true -> ets:file2tab(Gyrus);
							false-> ets:new(list_to_atom(Gyrus),[named_table,bag,public])
						end
					end,lists:seq(2,100)).



save_gyri() ->
	Named = lists:filter(fun(Tab)-> is_atom(Tab) end, ets:all() ),
	Gyri = lists:filter(fun(Tab)-> lists:sublist(atom_to_list(Tab),5)=="gyrus" end, Named),
	%io:format("Next tabs are about to save: ~n~p~n",[Gyri]),
	lists:foreach(  fun(Tab)-> 
						case ets:info(Tab,size)>0 of
							true -> ets:tab2file(Tab,"data/"++atom_to_list(Tab),[{extended_info,[object_count]}]);
							false-> ok %io:format("ets table ~p is empty~n",[Tab])
						end
					end,Gyri).



check_gyrus(J) -> Gyrus = bot:gyrus_name(J),
	Tabs = ets:all(),
	case lists:member(Gyrus,Tabs) of
		true -> ok;
		false-> ets:new(Gyrus,[named_table,bag,public])
	end.



new_gyrus(J) -> ets:new(bot:gyrus_name(J),[named_table,bag,public]).
