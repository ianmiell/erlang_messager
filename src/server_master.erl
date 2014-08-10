%% 
%% Responsible for starting up all the managing processes.
%%       
-module(server_master).
-include("listener_header.hrl").

-export([start/0, stop/0, init/0]).

start() ->
	spawn(?MODULE, init, []).

stop() ->
	?MODULE ! stop.

%% TODO: start links here
%% TODO: resiliency of modules
init() ->
	SelfPid = self(),
	register(?MODULE, self()),
	?LOG_SERVER:start(?LOG_LEVEL),
	acceptor:start(),
	message_processor:start(),
	%router_client:start(),
	?LOG_SERVER ! {log,1,SelfPid,"Done initialising, looping",[]},
	loop().

%%
loop() ->
	SelfPid = self(),
	receive
		stop ->
			?LOG_SERVER ! {log,1,SelfPid,"Master told to stop, ending",[]},
			acceptor:stop(),
			client:stop(),
			?LOG_SERVER:stop(),
			ok;
		X ->
			?LOG_SERVER ! {log,1,SelfPid,"??: ~p",[X]}
	end.

