%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gyri).
-export([save_gyri/0, new_gyrus/1, check_gyrus/1
		]).



save_gyri() ->
	Named = lists:filter(fun(Tab)-> is_atom(Tab) end, ets:all() ),
	Gyri = lists:filter(fun(Tab)-> lists:sublist(atom_to_list(Tab),5)=="gyrus" end, Named),
	io:format("Next tabs are about to save: ~n~p~n",[Gyri]),
	lists:foreach(  fun(Tab)-> ets:tab2file(Tab,"data/"++atom_to_list(Tab))
					end,Gyri).



check_gyrus(J) -> Gyrus = bot:gyrus_name(J),
	Tabs = ets:all(),
	case lists:member(Gyrus,Tabs) of
		true -> ok;
		false-> ets:new(Gyrus,[named_table,bag])
	end.



new_gyrus(J) -> ets:new(bot:gyrus_name(J),[named_table,bag]).
