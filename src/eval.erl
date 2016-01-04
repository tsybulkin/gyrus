


-module(eval).
-export([change_evaluation/3
		]).


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



% internal representation of the state. 
% Eval = {Vert,Hor,Diag1,Diag2,Counters}
change_evaluation(no_evaluation, {8,8}, blacks) -> 
	{2,{8,8},Board} = state:init_state([{8,8}]),
	init_evaluation(Board);

change_evaluation({Vert,Hor,D1,D2,Cnts}, {I,J}, OppColor) -> 
	%io:format("before move:~p~n",[dict:to_list(Cnts)]),
	Column = element(I,Vert),
	{Cnts1,Column1} = lists:foldl(fun(N,{Dict,Col})->
		S = element(N,Col),
		S1 = fiver:change_state(S,OppColor),
		Col1 = erlang:delete_element(N,Col),
		Col2 = erlang:insert_element(N,Col1,S1),
		{dict:update_counter(S1,1,dict:update_counter(S,-1,Dict)),Col2}
	end,{Cnts,Column},lists:seq(max(J-4,1),min(J,11))),
	Vert1 = erlang:delete_element(I,Vert),
	Vert2 = erlang:insert_element(I,Vert1,Column1),
	%io:format("after vert:~p~n",[dict:to_list(Cnts1)]),

	Row = element(J,Hor),
	{Cnts2,Row1} = lists:foldl(fun(N,{Dict,R})->
		S = element(N,R),
		S1 = fiver:change_state(S,OppColor),
		R1 = erlang:delete_element(N,R),
		R2 = erlang:insert_element(N,R1,S1),
		{dict:update_counter(S1,1,dict:update_counter(S,-1,Dict)),R2}
	end,{Cnts1,Row},lists:seq(max(I-4,1),min(I,11) )),
	Hor1 = erlang:delete_element(J,Hor),
	Hor2 = erlang:insert_element(J,Hor1,Row1),
	%io:format("after hor:~p~n",[dict:to_list(Cnts2)]),

	D1_index = J-I+11,
	if 
		D1_index<1 orelse D1_index>21 -> 
			Cnts3=Cnts2, D12=D1;
		true -> 
			Diag1=element(D1_index,D1), Size1=size(Diag1),
			if I < J -> Ind1=I; true -> Ind1=J end,
			{Cnts3,Diag11} = lists:foldl(fun(N,{Dict,Dia})->
				S = element(N,Dia),
				S1 = fiver:change_state(S,OppColor),
				Dia1 = erlang:delete_element(N,Dia),
				Dia2 = erlang:insert_element(N,Dia1,S1),
				{dict:update_counter(S1,1,dict:update_counter(S,-1,Dict)),Dia2}
			end,{Cnts2,Diag1},lists:seq(max(Ind1-4,1),min(Ind1,Size1) )),
			D11 = erlang:delete_element(D1_index,D1),
			D12 = erlang:insert_element(D1_index,D11,Diag11)
	end,
	%io:format("after diag1:~p~n",[dict:to_list(Cnts3)]),

	D2_index = J+I-5,
	if 
		D2_index<1 orelse D2_index>21 -> 
			Cnts4=Cnts3, D22=D2;
		true -> 
			Diag2=element(D2_index,D2), Size2=size(Diag2),
			if I+J < 15 -> Ind2=I; true -> Ind2=16-J end,
			{Cnts4,Diag12} = lists:foldl(fun(N,{Dict,Dia})->
				S = element(N,Dia),
				S1 = fiver:change_state(S,OppColor),
				Dia1 = erlang:delete_element(N,Dia),
				Dia2 = erlang:insert_element(N,Dia1,S1),
				{dict:update_counter(S1,1,dict:update_counter(S,-1,Dict)),Dia2}
			end,{Cnts3,Diag2},lists:seq(max(Ind2-4,1),min(Ind2,Size2) )),
			D21 = erlang:delete_element(D2_index,D2),
			D22 = erlang:insert_element(D2_index,D21,Diag12)
	end,
	%io:format("~p~n",[dict:to_list(Cnts4)]),
	{Vert2,Hor2,D12,D22,Cnts4}.
