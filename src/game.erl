%
%	Gomoku 
%	Continued: September 2015
%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(game).
-export([game_manager/2,
		start_new_game/3,
		start_new_game/2]).

-define(CONCURRENT_GAMES_LIMIT, 11).



game_manager(Schedule,WS) -> game_manager(Schedule,WS,0,0,0,0,0).

game_manager(Schedule,WS,CurrPlayersNbr,Won,Draw,Lost,GamesDone) ->
	receive
		{new_game_request, Color, Level, Pid} ->
			case CurrPlayersNbr < ?CONCURRENT_GAMES_LIMIT of
				true ->
					spawn(?MODULE,start_new_game,[Schedule,Pid]),
					game_manager(Schedule,WS,CurrPlayersNbr+1,Won,Draw,Lost,GamesDone);
				false ->
					Pid ! too_many_players,
					game_manager(Schedule,WS,CurrPlayersNbr,Won,Draw,Lost,GamesDone)
			end;

		{connection_closed, Game} ->
			game_manager(Schedule,WS,CurrPlayersNbr-1,Won+1,Draw,Lost,GamesDone+1);

		{draw, Game, Pid} ->
			game_manager(Schedule,WS,CurrPlayersNbr-1,Won,Draw+1,Lost,GamesDone+1);

		{opponent_won, Game, Pid} ->
			game_manager(Schedule,WS,CurrPlayersNbr-1,Won,Draw,Lost+1,GamesDone+1);

		{agent_won, Game, Pid} ->
			game_manager(Schedule,WS,CurrPlayersNbr-1,Won,Draw,Lost+1,GamesDone+1);

		{game_over, Game} -> 
			game_manager(Schedule,WS,CurrPlayersNbr-2,Won,Draw,Lost,GamesDone+1)
	after
		1000 ->
			if CurrPlayersNbr < ?CONCURRENT_GAMES_LIMIT -> 
				spawn(?MODULE,start_new_game,[Schedule]),
				game_manager(Schedule,WS,CurrPlayersNbr+2,Won,Draw,Lost,GamesDone);
			true ->
				game_manager(Schedule,WS,CurrPlayersNbr,Won,Draw,Lost,GamesDone)
			end
	end.



start_new_game(Schedule,GameManager) -> %% run Agent vs. Agent
	% run game

	GameManager ! {game_over, 12}.

start_new_game(Schedule,GameManager,Pid) -> %% run Agent vs. human
	% run game

	GameManager ! {draw, 12, self()}.



