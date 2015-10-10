%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(state).
-export([init_state/0]).




board_to_position(Board) ->
	Rows = tuple_to_list(Board),
	{Y,Rows1} = remove_empty_rows(Rows,0),
	{_,Rows2} = remove_empty_rows(lists:reverse(Rows1)),

	{X1,X2} = find_empty_columns(Rows2,15,1),
	{ X1,Y,board_to_position(Rows2,X1,X2,[]) }.

board_to_position([Row|Rows1],X1,X2,Acc) ->
	Tup = list_to_tuple(
		lists:sublist(tuple_to_list(Row),X1,X2-X1+1)
		),
	board_to_position([Row|Rows1],X1,X2,[Tup|Acc]);
board_to_position([],_X1,_X2,Acc) -> list_to_tuple(Acc).




remove_empty_rows([{e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}|Rows],Y) ->
	remove_empty_rows(Rows,Y+1);
remove_empty_rows(Rows,Y) -> {Y,Rows}.




find_empty_columns(Rows,15,1) ->





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

