%% Author: pascalbeaudry
%% Created: 5 mars 2009
%% Description: TODO: Add description to Velocity
-module(velocity).

%%
%% Include files
%%
-import(modulePlayer,[getLaserScan/1, getLaserScanSize/0]).

%%
%% Exported Functions
%%
-export([velocityNode/0,loopVelocityNode/0]).

%%
%% API Functions
%%
loopVelocityNode() ->
	receive
		{run, PIDnext} ->
			PIDnext ! velocityNode(),
			velocityp ! {run,PIDnext },
			loopVelocityNode();
		{stop} ->0
	end.


velocityNode() ->
	SizeLaser = getLaserScanSize(),
	Index = goodIndex(SizeLaser),

	Front = getFrontScan(Index),

	BackwardVelocity=100,
	FrontDistance=300.0,
	BackDistance=1000.0,

	Back=1000.00,

	SafeVelocity= 400,

	Msg=velocityMsg(Front,FrontDistance,Back,BackDistance,SafeVelocity,BackwardVelocity),

	Msg.

%%
%% Local Functions
%%

goodIndex(Size) when Size > 1 -> ((Size - 1) div 2);
goodIndex(_) -> 0.

getFrontScan(Index) when Index > 0 -> getLaserScan(Index);
getFrontScan(_) -> 0.

velocityMsg(Front,FrontDistance,_,_,SafeVelocity,_) when Front > (2*FrontDistance-200) ->
	{safeVelocity,SafeVelocity};
velocityMsg(Front,FrontDistance,Back,BackDistance,_,BackwardVelocity) when Front < FrontDistance ->
	backVelocityMsg(Front,FrontDistance,Back,BackDistance,BackwardVelocity);
velocityMsg(Front,FrontDistance,_,_,SafeVelocity,_) ->
	Scale= (SafeVelocity*(Front-FrontDistance)) div (FrontDistance-200),
	{safeVelocity,Scale}.


backVelocityMsg(_,_,Back,BackDistance,BackwardVelocity) when Back > BackDistance -> {safeVelocity,BackwardVelocity};
backVelocityMsg(Front,FrontDistance,Back,BackDistance,BackwardVelocity)->
	VelocityValue=(((FrontDistance-Front)-(BackDistance-Back)) div 1000) *  BackwardVelocity,
	{safeVelocity,VelocityValue}.

