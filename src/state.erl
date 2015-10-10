%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(state).
-export([init_state/0, t/0,
		board_to_position/1]).



board_to_position(Board) ->
	Rows = tuple_to_list(Board),
	{Y,Rows1} = remove_empty_rows(Rows,0),
	{_,Rows2} = remove_empty_rows(lists:reverse(Rows1),0),
	
	{X1,X2} = find_empty_columns(Rows2,15,1),
	{ X1-1,Y,board_to_position(Rows2,X1,X2,[]) }.

board_to_position([Row|Rows1],X1,X2,Acc) ->
	Tup = list_to_tuple(
		lists:sublist(tuple_to_list(Row),X1,X2-X1+1)
		),
	board_to_position(Rows1,X1,X2,[Tup|Acc]);
board_to_position([],_X1,_X2,Acc) -> list_to_tuple(Acc).




remove_empty_rows([{e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}|Rows],Y) ->
	remove_empty_rows(Rows,Y+1);
remove_empty_rows(Rows,Y) -> {Y,Rows}.




find_empty_columns([Row|Rows],MinInd,MaxInd) ->
	case [ J || J<-lists:seq(1,MinInd), element(J,Row) =/= e] of
		[] -> MinInd1 = MinInd;
		[Jmin|_] -> MinInd1 = min(MinInd,Jmin)
	end,
	case lists:reverse([ J || J<-lists:seq(MaxInd,15), element(J,Row) =/= e]) of
		[] -> MaxInd1 = MaxInd;
		[Jmax|_] -> MaxInd1 = max(MaxInd,Jmax)
	end,
	find_empty_columns(Rows,MinInd1,MaxInd1);
find_empty_columns([],MinInd,MaxInd) -> {MinInd,MaxInd}.




init_state() ->
	{1,
	{{e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}, 
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}}
	 }.



%%%%%%  T E S T S  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t() ->
	Board = {
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}, 
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,w,e,e,e,e,e,b,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,b,e,e,e,e},
	 {e,e,e,e,b,e,e,w,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}},

	 case St=board_to_position(Board) of
	 	{4,6,{
			 {e,w,e,e,e,e,e,b},
			 {e,e,e,e,e,e,b,e},
			 {b,e,e,w,e,e,e,e}}} -> ok;
		_ -> io:format("Wrong: ~p~n",[St])
	end.

