%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(test).
-export([t/0
		]).


t() ->
	B1 = 
	{{e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}, 
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,b,b,e,e,e,e,e},
	 {e,e,e,e,e,e,e,b,w,w,e,e,e,e,e},
	 {e,e,e,e,e,e,e,b,w,e,e,e,e,e,e},
	 {e,e,e,e,e,e,w,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}
	 },
	B2 = 
	{{e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}, 
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,b,b,e,e,e,e,e},
	 {e,e,e,e,e,e,e,b,w,w,e,e,e,e,e},
	 {e,e,e,e,e,e,e,b,w,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,w,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e},
	 {e,e,e,e,e,e,e,e,e,e,e,e,e,e,e}
	 },
	t1(position_extraction,B1),
	t2(position_reflection,B1),
	t3(position_rotation,B1),
	t4(back_transformation,B2),
	t5(transform,B2),
	t6(back_transform,B2).



t1(TestName,B1) ->
	{6,5,Position} = state:board_to_position(B1),
	True_pos = {{e,e,b,b},{e,b,w,w},{e,b,w,e},{w,e,e,e}},
	if Position =/= True_pos -> io:format("Test ~p failed. Position=~p~nCorrect:~p~n",[TestName,Position,True_pos]);
		true -> io:format("Test ~p passed * * *~n",[TestName])
	end.


t2(TestName,B1) ->
	{6,5,Position} = state:board_to_position(B1),
	{Refl_pos,-1} = state:reflect(Position,1),
	Correct2 = {{w,e,e,e},{e,b,w,e},{e,b,w,w},{e,e,b,b}},
	if Refl_pos =/= Correct2 -> io:format("Test ~p failed. Reflected position=~p~nCorrect:~p~n",[TestName,Refl_pos,Correct2]);
	 	true -> io:format("Test ~p passed * * *~n",[TestName])
	end.
	 

t3(TestName,B1) ->	
	{6,5,Position} = state:board_to_position(B1),
	{Rotated,2} = state:next_variant(Position,1),
	Correct3 = {{w,e,e,e},{e,b,b,e},{e,w,w,b},{e,e,w,b}},
	if Rotated =/= Correct3 -> io:format("Test ~p failed. Rotated position=~p~nCorrect:~p~n",[TestName,Rotated,Correct3]);
	 	true -> io:format("Test ~p passed * * *~n",[TestName])
	end.
	

t4(TestName,B2) ->
	{7,5,Position} = state:board_to_position(B2),
	{Rotated,2} = state:next_variant(Position,1),
	{X,Y} = {4,1},
	Moves = state:filter_legal([{X,Y}],7,5,2,Position,{7,B2}),
	[{X1,Y1}|_]=Moves,
	{X2,Y2} = {8,6},

	if {X2,Y2} =/= {X1,Y1} -> io:format("Test ~p failed. Moves=~p~nCorrect:~p~n",[TestName,Moves,{X2,Y2}]);
	 	true -> io:format("Test ~p passed * * *~n",[TestName])
	end.
	

t5(TestName,B2) ->
	{X,Y}={3,0},
	{7,5,Position} = state:board_to_position(B2),
	Trans = [{2,{5,3}},{3,{1,5}},{4,{0,1}},{-4,{0,3}},{-3,{1,0}},{-2,{5,1}},{-1,{3,5}}],

	lists:foreach(fun({Var,XY})-> 
		case state:transform(X,Y,Var,Position) of
			XY -> ok;
			{Xt,Yt} -> io:format("variant ~p failed. ~p=/=~p~n",[Var,XY,{Xt,Yt}])
		end
	end,Trans).
	

t6(TestName,B2) ->
	{X1,Y1}={3,0},
	{7,5,Position} = state:board_to_position(B2),
	BackTrans = [{2,{5,3}},{3,{1,5}},{4,{0,1}},{-4,{0,3}},{-3,{1,0}},{-2,{5,1}},{-1,{3,5}}],

	lists:foreach(fun({Var,{X,Y}})-> 
		case state:back_transform(X,Y,Var,Position) of
			{X1,Y1} -> ok;
			{Xt,Yt} -> io:format("variant ~p failed. ~p=/=~p~n",[Var,{X1,Y1},{Xt,Yt}])
		end
	end,BackTrans).
	


