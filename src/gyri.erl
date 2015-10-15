%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gyri).
-export([init_gyri/0]).


init_gyri() ->
	{ok,Ls} = file:list_dir("data"),
	lists:foreach(  fun(File) -> 
		case File of
			[$g,$y,$r,$u,$s|_] -> ets:file2tab(File);
			_ -> ok
		end
					end, Ls).