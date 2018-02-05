%% Author: pascalbeaudry
%% Created: 26 janv. 2009
%% Description: module to link behavior with player/stage

-module(modulePlayer).
-export([start/1, stop/0, init/1]).
-export([playerLaser/0, getLaserScan/1, getLaserScanSize/0, setSpeed/2, setCarLike/2]).

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    complex ! stop.

setSpeed(X,Y) ->
    call_port({setSpeed, X , Y}).

setCarLike(S,A) ->
    call_port({setCarLike, S,A}).

playerLaser() ->
    call_port({playerLaser}).

getLaserScan(Index) ->
    call_port({getLaserScan, Index}).

getLaserScanSize() ->
    call_port({getLaserScanSize}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
        {complex, Result} ->
            Result
    end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary]),
    loop(Port).

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, term_to_binary(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {complex, binary_to_term(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            exit(port_terminated)
    end.
