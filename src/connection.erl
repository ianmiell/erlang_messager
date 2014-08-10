-module(connection).
-include("listener_header.hrl").
-export([start/2]).

start(Socket,Num) ->
	B = list_to_binary("ev_id=" ++ integer_to_list(Num)),
	message_processor ! {new_connection,self(),[B]},
	Rem = Num rem ?PER_THREAD_LOGGING_MOD,
	case Rem of
		0 ->
			loop(Socket,true,Num,0);
		_ ->
			loop(Socket,false,Num,0)
	end.
	
loop(S,LogBool,Num,Count) ->
	inet:setopts(S,[{active,true}]),
	receive
		{tcp,S,Data} ->
			%gen_tcp:send(S,Data),
			loop(S,LogBool,Num,Count);
		{tcp_closed,S} ->
			message_processor ! {delete_connection,self()},
			?LOG_SERVER ! {log,10,self(),"Socket ~w closed [~w]~n",[S,self()],true};
		{new_message,Msg} ->
			gen_tcp:send(S,Msg),
			loop(S,LogBool,Num,Count);
		X ->
			message_processor ! {delete_connection,self()},
			?LOG_SERVER ! {log,1,self(),"Socket ~p: Received unknown: ~p~n",[S,X],true}
	after ?MESSAGE_SEND_DELAY ->
		{_,Minutes,Seconds} = time(),
		gen_tcp:send(S,integer_to_list(Minutes) ++ "," ++ integer_to_list(Seconds)),
		NewCount = Count+1,
		?LOG_SERVER ! {log,10,self(),"Socket ~p: Sent answer: ~p:~p~n",[S,time(),NewCount],LogBool},
		loop(S,LogBool,Num,NewCount)
	end.

