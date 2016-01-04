%
%   Gomoku-2 
%	Continued: December 2015
%	
%   This is an agent making decisions on the basis
%   of position evaluation. Neither learning, nor game tree search are used.
%   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(fiver).
-export([state/1,
		change_state/2,
		count/4,
		init_counters/0
		]).


state(Fiver) -> state(free,Fiver).

state(State,[e|Fiver]) -> state(State,Fiver);
state(free,[b|Fiver]) -> state(b_singlet,Fiver);
state(free,[w|Fiver]) -> state(w_singlet,Fiver);

state(mixed,_) -> mixed;
state(b_singlet,[w|_]) -> mixed;
state(w_singlet,[b|_]) -> mixed;
state(b_singlet,[b|Fiver]) -> state(b_duplet,Fiver);
state(w_singlet,[w|Fiver]) -> state(w_duplet,Fiver);

state(b_duplet,[w|_]) -> mixed;
state(w_duplet,[b|_]) -> mixed;
state(b_duplet,[b|Fiver]) -> state(b_triplet,Fiver);
state(w_duplet,[w|Fiver]) -> state(w_triplet,Fiver);

state(b_triplet,[w|_]) -> mixed;
state(w_triplet,[b|_]) -> mixed;
state(b_triplet,[b|Fiver]) -> state(b_quartet,Fiver);
state(w_triplet,[w|Fiver]) -> state(w_quartet,Fiver);

state(b_quartet,[w|_]) -> mixed;
state(w_quartet,[b|_]) -> mixed;
state(b_quartet,[b|Fiver]) -> state(b_quintet,Fiver);
state(w_quartet,[w|Fiver]) -> state(w_quintet,Fiver);

state(State,[]) -> State.



change_state(S,blacks) -> state(S,[b]);
change_state(S,whites)-> state(S,[w]).



count(V,H,D1,D2) ->
	Cnts0 = init_counters(),

	lists:foldl(fun(Tup,Dict)-> 
		lists:foldl(fun(Tup1,Dict1)->
			lists:foldl(fun(Key,Dict2)-> dict:update_counter(Key,1,Dict2) 
			end,Dict1,tuple_to_list(Tup1))
		end,Dict,tuple_to_list(Tup))
	end,Cnts0,[V,H,D1,D2]).


init_counters()	-> dict:from_list([{free,0},{mixed,0},{b_singlet,0},{w_singlet,0},
	{b_duplet,0},{w_duplet,0},{b_triplet,0},{w_triplet,0},
	{b_quartet,0},{w_quartet,0},{b_quintet,0},{w_quintet,0}]).

