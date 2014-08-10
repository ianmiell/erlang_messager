-module(router_client).

-export([start/0, init/0, listen/0, stop/0, get_message/2]).
-include("listener_header.hrl").

start() ->
	spawn(?MODULE, init, []).

init() ->
	error_logger:info_msg("router client starting..."),
	register(?MODULE, self()),
	listen().

stop() ->
	?MODULE ! stop.

listen() ->
	case gen_tcp:connect(?ROUTER_SERVER_NAME, ?ROUTER_SERVER_PORT, [list, {packet, line}, {recbuf, ?RECEIVE_BUFFER_LENGTH}], ?ROUTER_CLIENT_TIMEOUT) of
		{ok, Sock} ->
			?LOG_SERVER ! {log,10,self(),"Router client connected",[]},
			get_message(Sock);
		{error, X} ->
			receive
			after ?ROUTER_CLIENT_RETRY_TIME ->
					true
			end,
			error_logger:info_msg(X),
			listen()
	end.

get_message(Sock) ->
	get_message(Sock, []).

get_message(Sock, Acc) ->
	receive
		{tcp, Sock, Data} ->
			Acc2 = Acc ++ Data,
			case string:len(Data) of
				?RECEIVE_BUFFER_LENGTH ->
					case lists:nth(?RECEIVE_BUFFER_LENGTH, Data) of
						$\n -> forward_message(Acc2);
						_ -> get_message(Sock, Acc2)
					end;
				_ ->
					forward_message(Acc2),
					get_message(Sock)
			end;
		stop ->
			ok;
		X ->
			error_logger:error_msg(X),
			gen_tcp:shutdown(Sock, read_write),
			listen()
	after ?ROUTER_CLIENT_TIMEOUT ->
			gen_tcp:shutdown(Sock, read_write),
			listen()
	end.

% Gets a message and strips /r's and /n's out
get_stripped_message(Msg) ->
	[X || X <- Msg, X =/= $\r, X =/= $\n].


forward_message(Msg) ->
	StrippedMsg = get_stripped_message(Msg),
	MsgTokens = string:tokens(StrippedMsg," \t"),
	message_processor ! {new_message,StrippedMsg}.
