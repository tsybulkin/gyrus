%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(moves).
-export([get_mat/3, t/0, rate_line/2, rate_row/5,
		get_selected_moves/1,
		get_enforced_moves/3,
		get_good_moves/2,
		legal_move/2]).



get_selected_moves({Turn,Board}) ->
	Lines = lines:extract_lines(Board),
	case game:color(Turn) of
		whites -> Own = w, Opp = b;
		blacks -> Own = b, Opp = w
	end,

	case get_mat(Own,Opp,Lines) of
		not_found ->
			%io:format("Mat not found~n"),
			case get_enforced_moves(Own,Opp,Lines) of
				[] ->
					%io:format("No enforced moves found~n"),
					case get_good_moves(Own,Lines) of
						[] -> [rand:get_random_move(Board,1)];
						Ms -> Ms
					end;
				Ms -> Ms
			end;
		Moves -> Moves
	end.



get_mat(Own,Opp,Lines) ->
	case lines:find_4(Own,Lines) of
		not_found -> 
			case lines:find_4(Opp,Lines) of
				[] ->
					case lines:find_open3(Own,Lines) of
						[] -> not_found;
						Moves -> Moves
					end;
				Moves -> Moves
			end;
		Moves -> Moves
	end.



get_enforced_moves(Own,Opp,Lines) -> 
	Open3 = lines:prevent_open3(Opp,Lines),
	case Open3 of
		[] -> [];
		_ -> lists:usort(lines:find_covered3(Own,Lines)++Open3)
	end.
	



t() ->
	Board = 
	{{e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}, 
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,w,e,e,w,e,e,e,e,e,e},
	 {e,e,e,e,e,e,b,b,b,e,e,e,e,e,e},
	 {e,e,e,e,e,b,e,b,w,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,w,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}},

	 Lines = lines:extract_lines(Board),
	 get_good_moves(b,Lines).




get_good_moves(Me,Lines) ->
	Moves = lists:foldl(fun(Line,Acc) -> rate_line(Me,Line)++Acc end,[],Lines), 
	Sorted = lists:sort(fun({_,A},{_,B})-> A>B end, group(lists:sort(Moves))),
	[{_,MaxRate}|_] = Sorted, Thr = 0.7*MaxRate,
	Best = pick_best(lists:filter(fun({_,R})-> R>=Thr end,Sorted),Thr+1),
	shuffle(Best).


group([]) -> [];
group([{Move,R}|Ls]) -> group(Ls,Move,R,[]).
group([{Move,R}|Ls], Move,Cr,Acc) -> group(Ls,Move,Cr+R,Acc);
group([{Move1,R}|Ls],Move,Cr,Acc) -> group(Ls,Move1,R,[{Move,Cr}|Acc]);
group([],Move,Cr,Acc) -> [{Move,Cr}|Acc].



rate_line(Me,{{X1,Y1},{X2,Y2},Stones}) ->
	if X2>X1 -> Dx=1; X2<X1 -> Dx=-1; true -> Dx=0 end,
	if Y2>Y1 -> Dy=1; Y2<Y1 -> Dy=-1; true -> Dy=0 end,
	Rated_moves = scan_line(Me,Stones,1,[],undef,[]),
	[ { {X1+Dx*(Index-1),Y1+Dy*(Index-1)}, R} || {Index,R} <- Rated_moves].

scan_line(Me,Stones,Index,Row,Color,Acc) when length(Row)=:=5 -> 
	Row1 = lists:droplast(Row), 
	case length(lists:filter(fun(S)-> S=/=e end,Row)) of
		0 -> scan_line(Me,Stones,Index,Row1,undef,Acc);
		_ -> scan_line(Me,Stones,Index,Row1,Color,rate_row(Me,Color,Row,Index-1,Acc))
	end;
	
scan_line(Me,[e|Stones],Index,Row,undef,Acc) -> scan_line(Me,Stones,Index+1,[e|Row],undef,Acc);
scan_line(Me,[S|Stones],Index,Row,undef,Acc) -> scan_line(Me,Stones,Index+1,[S|Row],S,Acc);
scan_line(Me,[e|Stones],Index,Row,Color,Acc) -> scan_line(Me,Stones,Index+1,[e|Row],Color,Acc);
scan_line(Me,[S|Stones],Index,Row,S,Acc) -> scan_line(Me,Stones,Index+1,[S|Row],S,Acc);
scan_line(Me,[S|Stones],Index,Row,_,Acc) -> 
	E_row = take_e(Row),
	scan_line(Me,Stones,Index+1,[S|E_row],S,Acc);
scan_line(_Me,[],_,_,_,Acc) -> [ {Ind,math:sqrt(R)} || {Ind,R}<- group(lists:sort(Acc))].


take_e(Row) -> take_e(Row,[]).
take_e([e|Row],Acc) -> take_e(Row,[e|Acc]);
take_e(_,Acc) -> Acc.


rate_row(b,blacks,[e|Row],Index,Acc) -> rate_row(b,blacks,Row,Index-1,[{Index,1.5}|Acc]);
rate_row(w,whites,[e|Row],Index,Acc) -> rate_row(w,whites,Row,Index-1,[{Index,1.5}|Acc]);
rate_row(Me,Color,[e|Row],Index,Acc) -> rate_row(Me,Color,Row,Index-1,[{Index,1}|Acc]);
rate_row(Me,Color,[_|Row],Index,Acc) -> rate_row(Me,Color,Row,Index-1,Acc);
rate_row(_,_,[],_,Acc) -> Acc.



pick_best(Ls,_Thr) when length(Ls)=<15 -> [XY ||{XY,_} <- Ls];
pick_best(Ls,Thr) ->
	Ls1 = lists:filter(fun({_,X})-> X>Thr end, Ls),
	if
		length(Ls1)>15 -> pick_best(Ls1,Thr+1);
		length(Ls1)<9 -> [XY ||{XY,_} <- Ls];
		true -> [XY ||{XY,_} <- Ls1]
	end.



shuffle(Moves) -> 
	random:seed(now()),
	Shuffled = lists:zip([random:uniform() || _ <- lists:seq(1,length(Moves))], Moves),
	[ Move || {_,Move} <- lists:sort(Shuffled)].



legal_move({I,J},Board) ->
	case element(I,element(J,Board)) of
		e -> true;
		_ -> false
	end.

