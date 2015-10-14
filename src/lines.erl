%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lines).
-export([check_five/1,
		extract_lines/1,
		pick_5th/2,
		pick_4th/2,
		find_4/2,
		find_open3/2,
		find_covered3/2
		]).



check_five(illegal_move) -> illegal_move;
check_five({Turn,Board}) -> 
	case game:color(Turn) of
		blacks -> check_five(board, w,Board);
		whites -> check_five(board, b,Board)
	end.

check_five(board,Color,Board) ->
	check_five(lines,Color,extract_lines(Board));

check_five(lines,Color,[{_,_,Line}|Lines]) ->
	case check_five_in_line(0,Color,Line) of
		true -> true;
		false-> check_five(lines,Color,Lines)
	end;
check_five(lines,_,[]) -> false.

check_five_in_line(5,_,_) -> true;
check_five_in_line(Count,Color,[Stone|Line]) ->
	case Stone == Color of
		true -> check_five_in_line(Count+1,Color,Line);
		false-> check_five_in_line(0,Color,Line)
	end;
check_five_in_line(_,_,[]) -> false.

	

extract_lines(Board) ->
	extract_vert_lines(15,Board) ++
	extract_hor_lines(15,Board) ++
	extract_diagonals(Board).


extract_vert_lines(0,_) -> [];
extract_vert_lines(N,Board) -> [{ {N,15}, {N,1}, lists:foldl(fun(I,Acc) -> [element(N,element(I,Board))|Acc] 
				end, [], lists:seq(1,15)) } | extract_vert_lines(N-1,Board) ].

extract_hor_lines(0,_) -> [];
extract_hor_lines(N,Board) -> 
	[ { {1,N}, {15,N}, tuple_to_list(element(N, Board)) } | extract_hor_lines(N-1,Board) ].


extract_diagonals(Board) ->
	[{{1,1-H},{15+H,15},[ element(I, element(I-H,Board)) || I <- lists:seq(1,15+H) ]} || H <- lists:seq(-10,0) ] ++
	[{{1+H,1},{15,15-H},[ element(I, element(I-H,Board)) || I <- lists:seq(1+H,15) ]} || H <- lists:seq(1,10) ] ++
	[{{1,H-1},{H-1,1},[ element(I, element(H-I,Board)) || I <- lists:seq(1,H-1) ]} || H <- lists:seq(6,16) ] ++
	[{{H-15,15},{15,H-15},[ element(I, element(H-I,Board)) || I <- lists:seq(H-15,15) ]} || H <- lists:seq(17,26) ].
	


pick_5th(Color,{{X1,Y1},{X2,Y2},Stones}) ->
	case pick_5th(Color,Stones,1) of
		not_found -> not_found;
		Index ->
			if X2>X1 -> Dx=1; X2<X1 -> Dx=-1; true -> Dx=0 end,
			if Y2>Y1 -> Dy=1; Y2<Y1 -> Dy=-1; true -> Dy=0 end,

			{X1+Dx*(Index-1),Y1+Dy*(Index-1)}
	end.

pick_5th(Me,[Me,Me,Me,Me,e|_],Index) -> Index + 4;
pick_5th(Me,[Me,Me,Me,e,Me|_],Index) -> Index + 3;
pick_5th(Me,[Me,Me,e,Me,Me|_],Index) -> Index + 2;
pick_5th(Me,[Me,e,Me,Me,Me|_],Index) -> Index + 1;
pick_5th(Me,[e,Me,Me,Me,Me|_],Index) -> Index;
pick_5th(_,[_,_,_,_],_) -> not_found;
pick_5th(Me,[_|Stones],Index) -> 
	pick_5th(Me,Stones,Index+1);
pick_5th(_,Stones,_) when length(Stones)<5 -> not_found.



pick_4th(Color,{{X1,Y1},{X2,Y2},Stones}) ->
	if X2>X1 -> Dx=1; X2<X1 -> Dx=-1; true -> Dx=0 end,
	if Y2>Y1 -> Dy=1; Y2<Y1 -> Dy=-1; true -> Dy=0 end,

	[ {X1+Dx*(Index-1),Y1+Dy*(Index-1)} || Index <- pick_4th(Color,Stones,1) ].
	
pick_4th(Me,[e,Me,Me,Me,e,e|Tile],Index) -> [Index + 4|pick_4th(Me,[e,e|Tile],Index+4)];
pick_4th(Me,[e,Me,Me,e,Me,e|Tile],Index) -> [Index + 3|pick_4th(Me,[e,Me,e|Tile],Index+3)];
pick_4th(Me,[e,Me,e,Me,Me,e|Tile],Index) -> [Index + 2|pick_4th(Me,[e,Me,Me,e|Tile],Index+2)];
pick_4th(Me,[e,e,Me,Me,Me,e|Tile],Index) -> [Index + 1|pick_4th(Me,[e,Me,Me,Me,e|Tile],Index+1)];
pick_4th(_,[_,_,_,_,_],_) -> [];
pick_4th(_,Stones,_) when length(Stones)<5 -> [];
pick_4th(Me,[_|Stones],Index) -> pick_4th(Me,Stones,Index+1).




find_4(Color, [Line|Lines]) -> 
	case pick_5th(Color,Line) of
		not_found ->
			find_4(Color,Lines);
		{X,Y} -> {X,Y}
	end;
find_4(_,[]) -> not_found.



find_open3(Color,[Line|Lines]) -> 
	pick_4th(Color,Line) ++ find_open3(Color,Lines);
find_open3(_,[]) -> [].


find_covered3(Color,[Line|Lines]) ->
	pick_cvd_4th(Color,Line) ++ find_covered3(Color,Lines);
find_covered3(_,[]) -> [].


pick_cvd_4th(Color,{{X1,Y1},{X2,Y2},Stones}) ->
	if X2>X1 -> Dx=1; X2<X1 -> Dx=-1; true -> Dx=0 end,
	if Y2>Y1 -> Dy=1; Y2<Y1 -> Dy=-1; true -> Dy=0 end,

	[ {X1+Dx*(Index-1),Y1+Dy*(Index-1)} || Index <- pick_cvd_4th(Color,Stones,1) ].
	


pick_cvd_4th(Me,[e,Me,Me,Me,e|Tile],Index) -> [ Index,Index+4 | pick_cvd_4th(Me,Tile,Index+5) ];
pick_cvd_4th(Me,[e,Me,Me,e,Me|Tile],Index) -> [Index + 3 | pick_cvd_4th(Me,[Me|Tile],Index+4) ];
pick_cvd_4th(Me,[e,Me,e,Me,Me|Tile],Index) -> [Index + 2 | pick_cvd_4th(Me,[Me,Me|Tile],Index+3) ];
pick_cvd_4th(Me,[e,e,Me,Me,Me|Tile],Index) -> [Index + 1 | pick_cvd_4th(Me,[Me,Me,Me|Tile],Index+2) ];
pick_cvd_4th(Me,[Me,e,Me,Me,e|Tile],Index) -> [Index + 1 | pick_cvd_4th(Me,[Me,Me,e|Tile],Index+2) ];
pick_cvd_4th(Me,[Me,Me,e,Me,e|Tile],Index) -> [Index + 2 | pick_cvd_4th(Me,[Me,e|Tile],Index+3) ];
pick_cvd_4th(Me,[Me,Me,Me,e,e|Tile],Index) -> [Index + 3 | pick_cvd_4th(Me,[e|Tile],Index+4) ];
pick_cvd_4th(_,[_,_,_,_],_) -> [];
pick_cvd_4th(Me,[_|Stones],Index) -> 
	pick_cvd_4th(Me,Stones,Index+1);
pick_cvd_4th(_,Stones,_) when length(Stones)<5 -> [].



