%% Author: pascalbeaudry
%% Created: 21 janv. 2009
%% Description: module to link behavior with player/stage
-module(modulePlayerPort).

-export([start/1, stop/0, init/1]).
-export([setSpeed/2, setCarLike/2]).

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).
stop() ->
    modulePlayer ! stop.

setSpeed(X,Y) ->
    call_port({setSpeed, X , Y}).
setCarLike(S,A) ->
    call_port({setCarLike, S,A}).

call_port(Msg) ->
    player ! {call, self(), Msg},
    receive
        {player, Result} ->
            Result
    end.

init(ExtPrg) ->
    register(player, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    Caller ! {player, decode(Data)}
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

encode({setSpeed, X, Y}) -> [1, X, Y];
encode({setCarLike, S, A}) -> [2, S, A].

decode([Int]) -> Int.

