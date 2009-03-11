%% Author: pascalbeaudry
%% Created: 11 mars 2009
%% Description: TODO: Add description to player
-module(player).

%%
%% Include files
%%
-import(modulePlayer,[setSpeed/2]).

%%
%% Exported Functions
%%
-export([loopPlayerNode/0]).

%%
%% API Functions
%%
loopPlayerNode() ->
	receive
		{avoid, Avoid} ->
			receive
				{safeVelocity, Velocity} ->
					mainp ! {ok,playerNode(Avoid,Velocity)},
					loopPlayerNode()
			end;
		{safeVelocity, Velocity} ->
			receive
				{avoid, Avoid} ->
					mainp ! {ok,playerNode(Avoid,Velocity)},
					loopPlayerNode()
			end;
		{stop} ->0;
		Other -> 
			io:format("Error PlayerNode msg : ~p\n", [Other]),
			loopPlayerNode()

	end.

playerNode(Avoid,Velocity)->
%%		io:format("AvoidNode : ~p\n", [Avoid]),
%%		io:format("VelocityNode : ~p\n", [Velocity]),

		Vel=Velocity,
		Rot=Avoid,		

		move(Vel,Rot)
.

%%
%% Local Functions
%%
move(Vel,Rot) when (not ( Vel == nil) and (not (Rot== nil))) ->
	Rv=Vel/1000.0,
	Rs=Rot*3.141592/180,
	modulePlayer:setSpeed(Rv,Rs),
	{Rv,Rs};
move(Vel,Rot) when ((Vel == nil) and (not (Rot== nil))) ->
	Rv=0,
	Rs=Rot*3.141592/180,
	modulePlayer:setSpeed(Rv,Rs),
	{Rv,Rs};
move(Vel,Rot) when (not( Vel == nil) and ((Rot== nil))) ->
	Rv=Vel/1000.0,
	Rs=0,
	modulePlayer:setSpeed(Rv,Rs),
	{Rv,Rs};
move(Vel,Rot) when (( Vel == nil) and ((Rot== nil))) ->
	Rv=0,
	Rs=0,
	modulePlayer:setSpeed(Rv,Rs),
	{Rv,Rs};
move(_,_) -> {0,0}.

