%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gyri).
-export([init_gyri/0, save_gyri/0, new_gyrus/1
		]).


init_gyri() ->
	{ok,Ls} = file:list_dir("data"),
	lists:foldl(  fun(File,Max) -> 
		case File of
			[$g,$y,$r,$u,$s|Nbr] -> 
				ets:file2tab(File),
				max(Max,list_to_integer(Nbr));
			_ -> Max
		end
					end, 1, Ls).



save_gyri() ->
	Gyri = lists:filter(fun(Tab)-> lists:sublist(atom_to_list(Tab),5)=="gyrus" 
						end, ets:all() ),
	lists:foreach(  fun(Tab)-> ets:tab2file(Tab,"data/"++atom_to_list(Tab))
					end,Gyri).



new_gyrus(J) -> ets:new(list_to_atom("gyrus"++integer_to_list(J)),[named_table,bag]).
