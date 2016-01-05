%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(moves).
-export([get_mat/3, %rate_line/2, rate_row/5,
		get_selected_moves/1,
		get_enforced_moves/3,
		get_good_moves/1,
		legal_move/2
		]).

-define(THRESHOLD,0.75).

-define(MY_SINGL,0).
-define(OPP_SINGL,-0.5).
-define(MY_DUPL,2).
-define(OPP_DUPL,-4).
-define(MY_TRIPL,7).
-define(OPP_TRIPL,-14).
-define(MY_QUART,26).
-define(OPP_QUART,-51).
-define(MY_QUINT,100).
-define(OPP_QUINT,-100).
-define(FREE,0).
-define(MIXED,0).


init_w() -> dict:from_list([{{free,blacks},?FREE},{{free,whites},?FREE},
	{{mixed,blacks},?MIXED},{{mixed,whites},?MIXED},
	{{b_singlet,blacks},?MY_SINGL},{{w_singlet,blacks},?OPP_SINGL},
	{{b_duplet,blacks},?MY_DUPL},{{w_duplet,blacks},?OPP_DUPL},
	{{b_triplet,blacks},?MY_TRIPL},{{w_triplet,blacks},?OPP_TRIPL},
	{{b_quartet,blacks},?MY_QUART},{{w_quartet,blacks},?OPP_QUART},
	{{b_quintet,blacks},?MY_QUINT},{{w_quintet,blacks},?OPP_QUINT},
	{{b_singlet,whites},?OPP_SINGL},{{w_singlet,whites},?MY_SINGL},
	{{b_duplet,whites},?OPP_DUPL},{{w_duplet,whites},?MY_DUPL},
	{{b_triplet,whites},?OPP_TRIPL},{{w_triplet,whites},?MY_TRIPL},
	{{b_quartet,whites},?OPP_QUART},{{w_quartet,whites},?MY_QUART},
	{{b_quintet,whites},?OPP_QUINT},{{w_quintet,whites},?MY_QUINT}]).



get_selected_moves({Turn,Board,_}=State) ->
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
					case get_good_moves(State) of
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
	



get_good_moves({Turn,_,Eval}=State) ->
	W = init_w(),
	MyColor = state:color(Turn),
	CandidateMoves = get_candidate_moves(State),
	RatedMoves = [ {Move,get_value(get_counters_after_move(Move,Eval,MyColor),W,MyColor)} 
		|| Move <- CandidateMoves],
	[{_,MaxRate}|_]=SortedMoves=lists:sort(fun({_,R1},{_,R2})-> R1>R2 end, RatedMoves),
	Shift = 50 - MaxRate, Thld = 50*?THRESHOLD,
	Selected = lists:filter(fun({_,R})-> R+Shift > Thld end,SortedMoves),
	%io:format("Good moves: ~p~n",[Selected]),
	[ M ||{M,_} <- Selected ].
	


get_candidate_moves({Turn,Board,_}) ->
	Size = 1+min(7,round(math:sqrt(Turn))),
	%Color = own_color(Turn),
	lists:usort(lists:foldl(fun({I,J},Acc)->
		case element(I,element(J,Board)) of
			e -> if abs(I-8)<Size andalso abs(J-8)<Size -> [{I,J}|Acc]; true -> Acc end;
			_ -> if abs(I-8)+1<Size andalso abs(J-8)+1<Size -> Acc; true -> get_arround({I,J},2,Board)++Acc end
		end
	end,[],[{I,J} || I<-lists:seq(1,15), J<-lists:seq(1,15) ])).
	


get_arround({X,Y},Size,Board) ->
	lists:filter(fun({X1,Y1})->
		if 
			X1<1 orelse Y1<1 orelse X1>15 orelse Y1>15 -> false;
			element(X1,element(Y1,Board)) =/= e -> false;
			true -> true
		end
	end, [{X-Size+I,Y-Size+J} || I<-lists:seq(1,2*Size-1), J<-lists:seq(1,2*Size-1)]).



get_counters_after_move({I,J},{Vert,Hor,D1,D2,Cnts},MyColor) ->
	%Cnts = fiver:init_counters(),
	Column = element(I,Vert),
	Cnts1 = lists:foldl(fun(N,Acc)->
		S = element(N,Column),
		S1 = fiver:change_state(S,MyColor),
		dict:update_counter(S1,1,dict:update_counter(S,-1,Acc))
	end,Cnts,lists:seq(max(J-4,1),min(J,11))),

	Row = element(J,Hor),
	Cnts2 = lists:foldl(fun(N,Acc)->
		S = element(N,Row),
		S1 = fiver:change_state(S,MyColor),
		dict:update_counter(S1,1,dict:update_counter(S,-1,Acc))
	end,Cnts1,lists:seq(max(I-4,1),min(I,11) )),

	D1_index = J-I+11,
	if 
		D1_index<1 orelse D1_index>21 -> Cnts3 = Cnts2;
		true -> 
			Diag1 = element(D1_index,D1), Size1 = size(Diag1),
			if I < J -> Ind1=I; true -> Ind1=J end,
			Cnts3 = lists:foldl(fun(N,Acc)->
				S = element(N,Diag1),
				S1 = fiver:change_state(S,MyColor),
				dict:update_counter(S1,1,dict:update_counter(S,-1,Acc))
			end,Cnts2,lists:seq(max(Ind1-4,1),min(Ind1,Size1) ))
	end,

	D2_index = J+I-5,
	if 
		D2_index<1 orelse D2_index>21 -> Cnts4 = Cnts3;
		true -> 
			Diag2=element(D2_index,D2), Size2 = size(Diag2),
			if I+J < 15 -> Ind2=I; true -> Ind2=16-J end,
			Cnts4 = lists:foldl(fun(N,Acc)->
				S = element(N,Diag2),
				S1 = fiver:change_state(S,MyColor),
				dict:update_counter(S1,1,dict:update_counter(S,-1,Acc))
			end,Cnts3,lists:seq(max(Ind2-4,1),min(Ind2,Size2) ))
	end,
	Cnts4.


get_value(Cnts,W,Color) ->
	lists:foldl(fun(Key,Acc)->
		Acc + dict:fetch(Key,Cnts)*dict:fetch({Key,Color},W)
	end,0,dict:fetch_keys(Cnts)).



 legal_move({I,J},Board) ->
 	case element(I,element(J,Board)) of
 		e -> true;
 		_ -> false
 	end.

