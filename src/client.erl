-module(client).
-include("listener_header.hrl").
-export([start/0,client/4]).

start() ->
	start(?NUM_CLIENTS).
start(0) ->
	ok;
start(Num) ->
	case Num rem ?PER_THREAD_LOGGING_MOD of
		0 ->
			spawn(?MODULE,client,[?LISTENER_PORT,"TEST",true,Num]);
		_ ->
			spawn(?MODULE,client,[?LISTENER_PORT,"TEST",false,Num])
	end,
	start(Num-1).

client(PortNo,Message,LogBool,Num) ->
	{ok,Socket} = gen_tcp:connect("localhost",PortNo,[{active,false},{packet,2}]),
	?LOG_SERVER ! {log,10,self(),"~p: ~p: sending: ~p~n",[Num,Socket,Message],LogBool},
	gen_tcp:send(Socket,Message),
	communicate(PortNo,Message,Socket,LogBool,Num,1).

communicate(PortNo,Message,Socket,LogBool,Num,Count) ->
	case Count of
		-1 ->
			ok;
		_ ->
			A = gen_tcp:recv(Socket,0),
			?LOG_SERVER ! {log,10,self(),"~p: ~p returned to ~p",[Socket,A,Num],LogBool},
			NewCount = Count + 1,
			communicate(PortNo,Message,Socket,LogBool,Num,NewCount)
	end.
