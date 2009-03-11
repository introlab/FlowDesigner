%% Author: pascalbeaudry
%% Created: 11 mars 2009
%% Description: TODO: Add description to mainFlow
-module(mainFlow).

%%
%% Include files
%%
-import(modulePlayer,[getLaserScan/1, getLaserScanSize/0, start/1, stop/0]).
-import(velocity,[loopVelocityNode/0]).
-import(avoid,[loopAvoidNode/0]).
-import(player,[loopPlayerNode/0]).

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%

mainLoop(PID1,PID2,PID_end) ->
	PID1 ! {run, PID_end},
	PID2 ! {run, PID_end}.

processLoop()->
	receive
		{ok ,{ V, A}}->
%%			mainLoop(avoidp,velocityp,playerp),
			io:format("Msg : ~p , ~p~n", [V,A]),
			processLoop();
		{stop} ->
			io:format("Msg : stop \n"),
			avoidp ! {stop},
			velocityp ! {stop},
			playerp ! {stop},
			modulePlayer:stop();
		Other ->
			io:format("Error Main msg : ~p\n", [Other]),
			processLoop()
	end
.

%%
%% Local Functions
%%

start() ->
	PidPlayer = spawn(fun player:loopPlayerNode/0 ),
	PidAvoid = spawn(fun avoid:loopAvoidNode/0 ),
	PidVelocity = spawn(fun velocity:loopVelocityNode/0),
	PidLoop = spawn(fun processLoop/0),	
	
	register(playerp, PidPlayer),
	register(avoidp, PidAvoid),
	register(velocityp, PidVelocity),
	register(mainp, PidLoop),
	
	modulePlayer:start("./extprg"),
	mainLoop(PidVelocity,PidAvoid,PidPlayer).

%%stop() -> mainp ! {stop}.
