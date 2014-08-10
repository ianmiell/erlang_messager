-module(message_processor).
-export([start/0,init/0,listen/0,stop/0]).
-include("listener_header.hrl").

%% Stores two types of info in the table:
%% 1) keyed on all the current pids that have tcp connections, mapped to their criteria
%% 2) keyed on all the criteria terms that have tcp connections
%%
%% They will be received as a list of binaries.
%%
%% Each string below will be a binary
%%
%% eg: {PID1,["ev_id=4","ev_oc_id=5"]}
%%     {PID2,["ev_mkt_id=6","ev_oc_id=5"]}
%%     {"ev_id=4",[PID1]}
%%     {"ev_oc_id=5",[PID1,PID2]}
%%     {"ev_mkt_id=6",[PID2]}

%% TODO: validation of input data

start() ->
	spawn(?MODULE, init, []).

init() ->
	register(?MODULE, self()),
	ets:new(?CONN_TABLE,[set,private,named_table]),
	listen().

stop() ->
	?MODULE ! stop.

%% An individual message from the router should be received as a list of name/value pairs, eg [{"ev_id","5"},{"ev_oc_id","6"}]
%% that are all strings.
listen() ->
	receive
		{new_message,Msg} ->
			%?LOG_SERVER ! {log,10,self(),"New message received: ~p",[Msg]},
			process_message(Msg),
			listen();
		{new_connection,FromPid,Criteria} ->
			%?LOG_SERVER ! {log,10,self(),"New connection received: From: ~p Criteria: ~p",[FromPid,Criteria]},
			new_connection(FromPid,Criteria),
			listen();
		{delete_connection,Pid} ->
			%?LOG_SERVER ! {log,10,self(),"Delete connection received: Pid: ~p",[Pid]},
			delete_connection(Pid),
			listen();
		Other ->
			?LOG_SERVER ! {log,10,self(),"Unknown received: ~p",[Other]},
			listen()
	end.

%% For each ev_id, ev_mkt_id and ev_oc_id name, we look for pids that match insert them into a list, uniqify the list - HOW?
%% and send them the message to each pid in turn.
%% TODO: urlencode/format message?
process_message(Msg) ->
	[UnsortedPids] = [get_pids_for_criteria(M) || M <- Msg],
	PidsToSendTo = lists:usort(UnsortedPids),
	%?LOG_SERVER ! {log,10,self(),"PidsToSendTo: ~p",[PidsToSendTo]},
	MsgStr = message_to_string(Msg),
	[send_msg(MsgStr,P) || P <- PidsToSendTo].

message_to_string(Msg) ->
	"DUMMY".
	


%% Returns the pids for a given criteria created from the passed-in name-value pair,eg {"ev_id","5"}
%% Note both parts must be strings
%% For each element in the list, if the first element matches "ev_id", "ev_oc_id", "ev_mkt_id" then create a binary
get_pids_for_criteria(NameValue) ->
	case NameValue of 
		{"ev_id",Num} ->
			B = list_to_binary("ev_id=" ++ Num),
			case ets:lookup(?CONN_TABLE,B) of
				[] ->
					[];
				[{B,Pids}] ->
					%?LOG_SERVER ! {log,10,self(),"Found binary ~p and Pids: ~p",[B,Pids]},
					Pids
			end;
		{"ev_mkt_id",Num} ->
			B = list_to_binary("ev_mkt_id=" ++ Num),
			case ets:lookup(?CONN_TABLE,B) of
				[] ->
					[];
				[{B,Pids}] ->
					%?LOG_SERVER ! {log,10,self(),"Found binary ~p and Pids: ~p",[B,Pids]},
					Pids
			end;
		{"ev_oc_id",Num} ->
			B = list_to_binary("ev_oc_id=" ++ Num),
			case ets:lookup(?CONN_TABLE,B) of
				[] ->
					[];
				[{B,Pids}] ->
					%?LOG_SERVER ! {log,10,self(),"Found binary ~p and Pids: ~p",[B,Pids]},
					Pids
			end
	end.
	

send_msg(MsgStr,Pid) ->
	%?LOG_SERVER ! {log,10,self(),"Sending message ~p to Pid: ~p",[MsgStr,Pid]},
	Pid ! {new_message,MsgStr}.
	

%% Receives a new connection and stores criteria in the table
new_connection(Pid,Criteria) ->
	ets:insert(?CONN_TABLE,{Pid,Criteria}),
	insert_criteria(Pid,Criteria).

insert_criteria(Pid,Criteria) ->
	case Criteria of
		[] -> 
			ok;
		[H|T] ->
			case ets:lookup(?CONN_TABLE,H) of
				[] ->
					ets:insert(?CONN_TABLE,{H,[Pid]});
				[{H,Pids}] ->
					ets:insert(?CONN_TABLE,{H,Pids ++ [Pid]})
			end,
			insert_criteria(Pid,T)
	end.


%% Delete connection from table.
%% Also, lookup each criteria it had
%% and remove that pid from the criteria.
delete_connection(Pid) ->
	case ets:lookup(?CONN_TABLE,Pid) of
		[] -> 
			ok;
		[{Pid,Criteria}] ->
			delete_criteria(Pid,Criteria),
			ets:delete(?CONN_TABLE,Pid)
	end.

delete_criteria(Pid,CriteriaList) ->
	case CriteriaList of
		[] -> 
			ok;
		[Criteria|T] ->
			%?LOG_SERVER ! {log,1,self(),"In delete_criteria: Pid: ~p Criteria: ~p",[Pid,Criteria]},
			case ets:lookup(?CONN_TABLE,Criteria) of
				[{Criteria,Pids}] ->
					ets:insert(?CONN_TABLE,{Criteria,lists:delete(Pid,Pids)});
				X ->
					?LOG_SERVER ! {log,1,self(),"In delete_criteria X: ~p Criteria: ~p",[X,Criteria]}
			end,
			delete_criteria(Pid,T)
	end.
