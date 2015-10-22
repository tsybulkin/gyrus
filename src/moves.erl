%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(moves).
-export([get_mat/3, t/0,
		get_selected_moves/1,
		get_enforced_moves/3,
		get_good_moves/1,
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
					case get_good_moves(Lines) of
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
	 get_good_moves(Lines).




get_good_moves(Lines) ->
	Moves = lists:foldl(fun(Line,Acc) -> rate_moves(Line)++Acc end,[],Lines),
	Sorted = lists:sort(fun({_,A},{_,B})-> A>B end, group(lists:sort(Moves))),
	[{_,MaxRate}|_] = Sorted, Thr = 0.3*MaxRate,
	pick_best(lists:filter(fun({_,R})-> R>=Thr end,Sorted),Thr+1).



group([{Move,R}|Ls]) -> group(Ls,Move,R,[]).
group([{Move,R}|Ls], Move,Cr,Acc) -> group(Ls,Move,Cr+R,Acc);
group([{Move1,R}|Ls],Move,Cr,Acc) -> group(Ls,Move1,R,[{Move,Cr}|Acc]);
group([],Move,Cr,Acc) -> [{Move,Cr}|Acc].



rate_moves({{X1,Y1},{X2,Y2},Stones}) ->
	if X2>X1 -> Dx=1; X2<X1 -> Dx=-1; true -> Dx=0 end,
	if Y2>Y1 -> Dy=1; Y2<Y1 -> Dy=-1; true -> Dy=0 end,
	Rated_moves = rate_moves(Stones,1,[]),
	[ { {X1+Dx*(Index-1),Y1+Dy*(Index-1)}, R} || {Index,R} <- Rated_moves].

rate_moves([e,e,e,e,e,e|Stones],J,Acc) -> rate_moves([e,e,e,e|Stones],J+2,Acc);
rate_moves([e,e,e,e,S,e|Stones],J,Acc) -> rate_moves([S,e|Stones],J+4,[{J+1,1},{J+2,2},{J+3,2},{J+5,1}|Acc]);
rate_moves([e,e,e,S,S,e|Stones],J,Acc) -> rate_moves([S,S,e|Stones],J+3,[{J,1},{J+1,2},{J+2,2},{J+5,1}|Acc]);
rate_moves([e,e,e,S,S|Stones],J,Acc) -> rate_moves([e,S,S|Stones],J+2,[{J+1,1},{J+2,1}|Acc]);

rate_moves([S,e,e,S,S|Stones],J,Acc) -> rate_moves([e,S,S|Stones],J+2,[{J+1,2},{J+2,2}|Acc]);
rate_moves([S,e,e,e,S|Stones],J,Acc) -> rate_moves([e,S|Stones],J+3,[{J+1,2},{J+2,1},{J+3,2}|Acc]);
rate_moves([S,S,e,e,S|Stones],J,Acc) -> rate_moves([e,S,S|Stones],J+3,[{J+2,2},{J+3,2}|Acc]);


rate_moves([e,S,e,e,e,e|Stones],J,Acc) -> rate_moves([e|Stones],J+4,[{J,1},{J+2,2},{J+3,2},{J+4,1}|Acc]);
rate_moves([e,S,S,e,e,e|Stones],J,Acc) -> rate_moves([e|Stones],J+4,[{J,1},{J+3,2},{J+4,2},{J+5,1}|Acc]);

rate_moves([S,S,e,e,e|Stones],J,Acc) -> rate_moves([e,e,e|Stones],J+2,[{J+1,1},{J+2,1}|Acc]);
rate_moves([_,S,e,e,e|Stones],J,Acc) -> rate_moves([S,e,e,e|Stones],J+1,Acc);
rate_moves([_,_,S,e,e|Stones],J,Acc) -> rate_moves([  S,e,e|Stones],J+2,Acc);
rate_moves([_,_,_,S,e|Stones],J,Acc) -> rate_moves([    S,e|Stones],J+3,Acc);
rate_moves([_,_,_,_,S|Stones],J,Acc) -> rate_moves([      S|Stones],J+4,Acc);

rate_moves(Stones,_,Acc) when length(Stones)<5 -> Acc.



%get_good_moves(Own,Opp,Lines) -> get_good_moves([],Own,Opp,Lines).
%get_good_moves(Rated_moves,Own,Opp,[{{X1,Y1},{X2,Y2},Stones}|Lines]) ->
%	Rated_moves1 = get_good_moves(Rated_moves,Own,Opp,{X1,Y1},{X2,Y2},Stones),
%	get_good_moves(Rated_moves1,Own,Opp,Lines);
%get_good_moves([],_,_,[]) -> [];
%get_good_moves(Rated_moves,_,_,[]) ->
%	Thr = 0.3*lists:max([ Rate || {_,Rate} <- Rated_moves ]),
%	Sorted = lists:sort(fun({_,A},{_,B})-> A<B end,Rated_moves),
%	pick_best(lists:filter(fun({_,R})-> R>=Thr end,Sorted),Thr+1).

	
%get_good_moves(Rated_moves,Own,Opp,{X1,Y1},{X2,Y2},Stones) ->
%	if X2>X1 -> Dx=1; X2<X1 -> Dx=-1; true -> Dx=0 end,
%	if Y2>Y1 -> Dy=1; Y2<Y1 -> Dy=-1; true -> Dy=0 end,
%
%	lists:foldl(fun({Index,Rate},Acc)->
%					{X,Y} = {X1+Dx*(Index-1),Y1+Dy*(Index-1)},
%					case lists:keyfind({X,Y},1,Acc) of
%						false -> [{{X,Y},Rate}|Acc];
%						{{X,Y},OldRate} -> lists:keyreplace({X,Y},1,Acc,{{X,Y},Rate+OldRate})
%					end
%				end, Rated_moves, rate([],Own,Opp,Stones,1)).


pick_best(Ls,_Thr) when length(Ls)=<11 -> [XY ||{XY,_} <- Ls];
pick_best(Ls,Thr) ->
	Ls1 = lists:filter(fun({_,X})-> X>Thr end, Ls),
	if
		length(Ls1)>11 -> pick_best(Ls1,Thr+1);
		length(Ls1)<5 -> [XY ||{XY,_} <- Ls];
		true -> [XY ||{XY,_} <- Ls1]
	end.


% rates moves. Returns a list of {index,rate} for a row of stones
%rate(Acc,_,_,Ls,_) when length(Ls)<5 -> aggregate(Acc);
%rate(Acc,_,_,[e,e,e,e,e],_) -> aggregate(Acc);
%rate(Acc,Own,Opp,[e,e,e,e,e|[S6|Tail] ],Index) -> rate(Acc,Own,Opp,[e,e,e,e,S6|Tail],Index+1);
%rate(Acc,Own,Opp,Ls,Index) when length(Ls)==5 ->
%	Own_N = length([ S || S <- Ls, S=:=Own]),
%	Opp_N = length([ S || S <- Ls, S=:=Opp]),
%	case {Own_N,Opp_N} of
%		{0,K} when K > 1 -> % single opponent's stone
%			aggregate( [ {I,1} || I <- get_empty_indices(Ls,Index)]++Acc );
%		{_,0} -> 
%			aggregate( [ {I,1} || I <- get_empty_indices(Ls,Index)]++Acc );
%		{_,_} ->
%			aggregate(Acc)
%	end;
%rate(Acc,Own,Opp,[S1,S2,S3,S4,S5|[S6|Tail] ],Index) ->
%	Ls = [S1,S2,S3,S4,S5],
%	Own_N = length([ S || S <- Ls, S=:=Own]),
%	Opp_N = length([ S || S <- Ls, S=:=Opp]),
%	case {Own_N,Opp_N} of
%		{0,K} when K > 1 -> % single opponent's stone
%			rate([ {I,1} || I <- get_empty_indices(Ls,Index)]++Acc,Own,Opp,[S2,S3,S4,S5,S6|Tail],Index+1);
%		{_,0} -> 
%			rate([ {I,1} || I <- get_empty_indices(Ls,Index)]++Acc,Own,Opp,[S2,S3,S4,S5,S6|Tail],Index+1);
%		{_,_} ->
%			rate(Acc,Own,Opp,[S2,S3,S4,S5,S6|Tail],Index+1)
%	end.
%
%
%aggregate(Ls) ->
%	IndexList = lists:usort([ I || {I,_} <- Ls]),
%	[ {I,lists:sum([ Rate || {_,Rate} <- lists:filter(fun({A,_})-> A=:=I end, Ls)]) } || I <- IndexList].



legal_move({I,J},Board) ->
	case element(I,element(J,Board)) of
		e -> true;
		_ -> false
	end.


%
%get_empty_indices(Ls,Index) ->
%	get_empty_indices([],Ls,Index).
%
%get_empty_indices(Acc,[e|Ls],Index) ->
%	get_empty_indices([Index|Acc],Ls,Index+1);
%get_empty_indices(Acc,[_|Ls],Index) ->
%	get_empty_indices(Acc,Ls,Index+1);
%get_empty_indices(Acc,[],_) -> Acc.

