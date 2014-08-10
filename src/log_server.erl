%% Logging server

-module(log_server).

-export([start/1, stop/0, init/1, log_server/1, log/5]).
-include("listener_header.hrl").

start(LogLevel) ->
	spawn(?LOG_SERVER,init,[LogLevel]).

stop() ->
	?LOG_SERVER ! stop.

init(LogLevel) ->
	register(?LOG_SERVER,self()),
	log_server(LogLevel).

log_server(LogLevel) ->
	receive 
		{log,MsgLevel,Pid,Msg,Args,LogBool} ->
			case LogBool of
				true ->
					log(LogLevel,MsgLevel,pid_to_list(Pid),Msg,Args),
					log_server(LogLevel);
				_ ->
					log_server(LogLevel)
			end;
		{log,MsgLevel,Pid,Msg,Args} ->
			log(LogLevel,MsgLevel,pid_to_list(Pid),Msg,Args),
			log_server(LogLevel);
		{log,MsgLevel,Pid,Msg} ->
			log(LogLevel,MsgLevel,pid_to_list(Pid),Msg,[]),
			log_server(LogLevel);
		{set_log_level,NewLevel} ->
			log_server(NewLevel);
		{stop} ->
			ok;
		A ->
			io:format("Received: ~p",[A])
	end.

log(LogLevel,MsgLevel,Pid,Msg,Args) when integer(LogLevel), integer(MsgLevel), LogLevel >= MsgLevel -> 
	error_logger:info_msg(Pid ++ ":" ++ Msg,Args);
	log(_,_,_,_,_) -> ok.
