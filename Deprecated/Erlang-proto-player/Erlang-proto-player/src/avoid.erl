%% Author: pascalbeaudry
%% Created: 10 mars 2009
%% Description: TODO: Add description to avoid
-module(avoid).

%%
%% Include files
%%
-import(modulePlayer,[getLaserScan/1, getLaserScanSize/0]).

%%
%% Exported Functions
%%
-export([avoidNode/0,loopAvoidNode/0]).


%%
%% API Functions
%%
loopAvoidNode() ->
	receive
		{run, PIDnext} ->
			PIDnext ! avoidNode(),
			avoidp ! {run,PIDnext },
			loopAvoidNode();
		{stop} ->0
	end.

avoidNode() ->
	DetectionRange = 1200.00,
	RotationValue = 120,
	SizeLaser = getLaserScanSize(),

	IndexFront = goodFrontIndex(SizeLaser),
	IndexLeft = goodLeftIndex(SizeLaser),
	IndexRight = goodRightIndex(SizeLaser),

	ScanFrontValue = getScan(IndexFront),
	ScanLeftValue = getScan(IndexLeft),
	ScanRightValue = getScan(IndexRight),

	MinValue=min(ScanFrontValue, min(ScanLeftValue,ScanRightValue)),
	
	Msg=move(MinValue,ScanFrontValue,ScanLeftValue,ScanRightValue,DetectionRange,RotationValue),	

	Msg.


%%
%% Local Functions
%%
goodFrontIndex(Size) when Size > 1 -> ((Size - 1) div 2);
goodFrontIndex(_) -> 0.

goodLeftIndex(Size) when Size > 1 -> (2*(Size - 1) div 3);
goodLeftIndex(_) -> 0.

goodRightIndex(Size) when Size > 1 -> ((Size - 1) div 3);
goodRightIndex(_) -> 0.

getScan(Index) when Index > 0 -> getLaserScan(Index);
getScan(_) -> 0.

min(A,B) when A > B ->B;
min(A,_) -> A.

move(Min,_,_,_,_,_) when Min == 0 ->  {avoid,nil};
move(Min,_,_,_,Detection,_) when Min > Detection -> {avoid,0};
move(_,_,Left,Right,_,Rotation) ->
	turn(Left,Right,Rotation).

turn(Left,Right,Rotation) when Left > Right -> {avoid,Rotation};
turn(_,_,Rotation) -> {avoid,-Rotation}.
