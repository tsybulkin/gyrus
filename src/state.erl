%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(state).
-export([init_state/0, init_state1/0, print_board/1, print_state/1,
		board_to_position/1,
		change_state/2,
		get_key/1,
		color/1,
		next_variant/2,
		back_transform/4, transform/4,
		reflect/2,
		filter_legal/6
		]).


change_state({Turn,Board,Eval},{I,J}) ->
	Color = color(Turn),
	e = element(I,element(J,Board)),

	%io:format("~nmove ~p: {~p,~p}~n",[Turn,I,J]),
	Row1 = erlang:delete_element(I,element(J,Board)),
	Board1 = erlang:delete_element(J,Board),
	case Color of
		whites -> Row2 = erlang:insert_element(I,Row1,w);
		blacks -> Row2 = erlang:insert_element(I,Row1,b)
	end,
	Next_state = {Turn+1,Board2=erlang:insert_element(J,Board1,Row2), eval:change_evaluation(Eval,{I,J},Color)},
	print_board(Board2),

	case lines:check_five(Next_state) of
		false when Turn =:= 59 -> draw;
		false -> Next_state;
		Fiver -> 
			case Color of
				blacks -> {blacks_won,Fiver};
				whites -> {whites_won,Fiver}
			end	
	end.	



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




get_key(Position) ->
	Cy = size(Position) + 1,
	Cx = size(element(1,Position)) + 1,
	{Ws,Bs} = lists:foldl(
		fun(Y,{Ws1,Bs1})-> 
			Row = element(Y,Position),
			lists:foldl(
				fun(X,{Ws2,Bs2})->
					case element(X,Row) of
						e -> {Ws2,Bs2};
						w -> {[abs(2*X-Cx)+abs(2*Y-Cy)|Ws2],Bs2};
						b -> {Ws2,[abs(2*X-Cx)+abs(2*Y-Cy)|Bs2]}
					end
				end, {Ws1,Bs1}, lists:seq(1,Cx-1))
		end,{[],[]},lists:seq(1,Cy-1)),
	{lists:sort(Ws),lists:sort(Bs)}.



filter_legal(Moves,X0,Y0,Variant,Position,State) -> 
	lists:foldl(fun({X,Y},Acc)-> 
					{X1,Y1} = back_transform(X,Y,Variant,Position),
					case legal_place(X0+X1,Y0+Y1,State) of
						true -> [{X0+X1,Y0+Y1}|Acc]; 
						false -> Acc; 
						error -> 
							io:format("Illegal move:~p, Variant:~p Position:~p~n",[{X,Y},Variant,Position]),
							io:format("Back rotated coords:~p, Final:~p~n",[{X1,Y1},{X0+X1,Y0+Y1}]),
							print_state(State),
							error(error)
					end
				end,[],Moves).



legal_place(X,Y,{_Turn,_Board,_}) when X<1; X>15; Y<1; Y>15 -> false;
legal_place(X,Y,{_,Board,_}) -> 
	case moves:legal_move({X,Y},Board) of
		false -> error;
		true -> true
	end.



next_variant(Position,Var) when Var =:= 4; Var=:= -1 -> reflect(Position,Var); % reflection
next_variant(Position,Var) -> % rotation
	Rows = tuple_to_list(Position),
	[Row|_] = Rows, 
	N = size(Row),
	Ls = lists:foldl(fun(J,Acc)->
					[list_to_tuple(lists:reverse([ element(J,Rw) || Rw <- Rows]))|Acc]
					end,[],lists:seq(1,N)),
	{list_to_tuple(lists:reverse(Ls)),Var+1}.

	

reflect(Position,Var) -> {list_to_tuple(lists:reverse(tuple_to_list(Position))),-Var}.


% X,Y - local coords in Position. Returns new local coords in variant symetrical position
transform(X,Y,Variant,Position) ->
	Cy = size(Position) + 1, Cx = size(element(1,Position)) + 1,
	case Variant < 0 of
		true -> {X1,Y1}=transform(X,Y,-Variant,Cx,Cy), {X1,cVar(Cx,Cy,Variant)-Y1};
		false-> transform(X,Y,Variant,Cx,Cy)
	end.
transform(X,Y,1,_Cx,_Cy) -> {X,Y};
transform(X,Y,4,Cx,_Cy) -> {Y,Cx-X};
transform(X,Y,3,Cx,Cy) -> {Cx-X,Cy-Y};
transform(X,Y,2,_Cx,Cy) -> {Cy-Y, X}.



back_transform(X,Y,Variant,Position) ->
	Cy = size(Position) + 1, Cx = size(element(1,Position)) + 1,
	case Variant < 0 of
		true -> back_transform(X,cVar(Cx,Cy,Variant)-Y,-Variant,Cx,Cy);
		false-> back_transform(X,Y,Variant,Cx,Cy)
	end.
back_transform(X,Y,1,_Cx,_Cy) -> {X,Y};
back_transform(X,Y,2,_Cx,Cy) -> {Y,Cy-X};
back_transform(X,Y,3,Cx,Cy) -> {Cx-X,Cy-Y};
back_transform(X,Y,4,Cx,_Cy) -> {Cx-Y, X}.



cVar(Cx,Cy,Variant) ->
	case Variant rem 2 of
		0 -> Cx;
		_ -> Cy
	end.
	

print_state({Turn,Board,_}) -> io:format("Turn:~p~n",[Turn]), print_board(Board).



print_board(Board) ->
	Rows = lists:reverse(tuple_to_list(Board)),
	print_rows(15,Rows),
	io:format("  1 2 3 4 5 6 7 8 9 0 1 2 3 4 5~n~n").

print_rows(N,[Row|Rows]) ->
	print_stones(" " ++ integer_to_list(N rem 10), tuple_to_list(Row)),
	print_rows(N-1,Rows);
print_rows(_,[]) -> ok.
	%io:format("~n").


print_stones(Acc,[Stone|Stones]) ->
	case Stone of
		e -> print_stones("|-"++Acc,Stones);
		b -> print_stones("X-"++Acc,Stones);
		w -> print_stones("O-"++Acc,Stones)
	end;
print_stones(Acc,[]) ->
	[E1,E2,_|Tile] = lists:reverse(Acc),
	io:format("~s~n",[[E1,E2|Tile]]).




init_state() ->
	Board=
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
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}},
	{1, Board, init_evaluation(Board) }.

init_state1() ->
	Board =
	{{e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}, 
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,b,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}},
	{2,Board,init_evaluation(Board) }.


 init_evaluation(Board) ->
	Lv = lines:extract_vert_lines(15,Board),
	V = list_to_tuple(lists:foldl(fun({_,_,L},Acc)->
		L1 = lists:reverse(L),
		[list_to_tuple([ fiver:state(lists:sublist(L1,J,5)) || J <- lists:seq(1,11)]) | Acc]
	end,[],Lv)),

	Lh = lines:extract_hor_lines(15,Board),
	H = list_to_tuple(lists:foldl(fun({_,_,L},Acc)->
		[list_to_tuple([ fiver:state(lists:sublist(L,J,5)) || J <- lists:seq(1,11)]) | Acc]
	end,[],Lh)),

	Ld1 = lists:reverse(lines:extract_diagonals1(Board)),
	D1 = list_to_tuple(lists:foldl(fun({_,_,L},Acc)->
		[list_to_tuple([ fiver:state(lists:sublist(L,J,5)) || J <- lists:seq(1,length(L)-4)]) | Acc]
	end,[],Ld1)),

	Ld2 = lists:reverse(lines:extract_diagonals2(Board)),
	D2 = list_to_tuple(lists:foldl(fun({_,_,L},Acc)->
		[list_to_tuple([ fiver:state(lists:sublist(L,J,5)) || J <- lists:seq(1,length(L)-4)]) | Acc]
	end,[],Ld2)),

	{V,H,D1,D2,fiver:count(V,H,D1,D2)}.



color(Turn) when Turn rem 2 =:= 0 -> whites;
color(_Turn) -> blacks.


