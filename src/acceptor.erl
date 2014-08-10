-module(acceptor).
-export([start/0,server/2]).
-include("listener_header.hrl").

start() ->
	start(?NUM_ACCEPTOR_THREADS,?LISTENER_PORT).

start(Num,LPort) ->
	case gen_tcp:listen(LPort,[{active, false},{packet,2}]) of
		{ok, ListenSock} ->
			start_servers(Num,ListenSock),
		{ok, Port} = inet:port(ListenSock),
			Port;
		{error,Reason} ->
			{error,Reason}
	end.

start_servers(0,_) ->
	ok;
start_servers(Num,LS) ->
	spawn(?MODULE,server,[LS,Num]),
	start_servers(Num-1,LS).

server(LS,Num) ->
	case gen_tcp:accept(LS) of
		{ok,S} ->
			connection:start(S,Num),
			server(LS,Num);
		Other ->
			?LOG_SERVER ! {log,5,self(),"~p: accept returned ~w - goodbye!~n",[Num,Other],true},
			ok
	end.
