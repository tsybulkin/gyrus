-module(ws_handler).
-export([init/2]).
-export([websocket_info/3]).
-export([websocket_handle/3]).

-record(state, {board}).

handle_client_msg(<<"init">>, Req, State) -> 
    {reply, {text, <<"ok">>}, Req, State}.

%% 

init(Req, _Opts) ->
    Board = cowboy_req:binding(board,Req),
    {cowboy_websocket, Req, #state{board = erlang:binary_to_atom(Board, latin1)}}.

websocket_info(_Msg, Req, State) ->
    {ok, Req, State}.

websocket_handle({text,Bin}, Req, State) ->
    Msg = jsx:decode(Bin),
    handle_client_msg(Msg, Req, State).

