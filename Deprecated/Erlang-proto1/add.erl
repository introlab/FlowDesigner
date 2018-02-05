-module(add).
-compile(export_all).
-include("dataFlow.hrl").

start(NodeName,Inputs,Outputs,Parameters) ->
    InputNames = ["INPUT"],
    OutputNames = ["OUTPUT"],
    Pid = spawn(fun() -> add_loop(NodeName,InputNames, OutputNames, Inputs, Outputs, Parameters) end),
    {Pid, register(NodeName,Pid)}.


rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} -> Response
	end.
	
add_list([H|T]) -> H + add_list(T);
add_list([]) -> 0.

	
add_loop(NodeName, InputNames, OutputNames, Inputs, Outputs, Parameters) ->
    
        io:format("+++generic Node handling start : ~p~n",[NodeName]),

        %Generic node handling
        Ret = dataFlow:generic_node_message_handler(NodeName, InputNames, OutputNames, Inputs, Outputs, Parameters),

        io:format("+++generic Node handling done : ~p~n",[NodeName]),

	receive 
            
        {From, {handled,Result}}->
            io:format("+++Message handled: ~p : ~p~n",[NodeName,Result]),
            From ! {self(), Result},
            add_loop(NodeName,InputNames, OutputNames, Inputs,Outputs,Parameters);
        
        {From, {calculateNode, OutputName, Count} } ->
            
            io:format("+++calculateNode called for Node : ~p~n",[NodeName]),
        
            %Get our inputs
            L = dataFlow:generic_get_all_inputs(Inputs),
        
            %DO SOMETHING!!!
            
            %Return result
            From ! {self(), {calculate,[]} },
            add_loop(NodeName,InputNames, OutputNames, Inputs,Outputs,Parameters);
                                       
        {From, Other} ->
            From ! {self(), {doesNotUnderstand, Other} },
            add_loop(NodeName,InputNames, OutputNames, Inputs,Outputs,Parameters);
            
         Other ->
            io:format("+++Unexpected message: ~p : ~p~n",[NodeName,Other]), 
            add_loop(NodeName,InputNames, OutputNames, Inputs,Outputs,Parameters)
	end.
        
       
        
test() ->
	{Pid1,State1} = add:start(add1,[],[],[]), 
        io:format("+++Node Started+++ Info = ~p~p~n",[Pid1,State1]),
        {Pid2, State2} = add:start(add2,[],[],[]),
        io:format("+++Node Started+++ Info = ~p~p~n",[Pid2,State2]),
        add:rpc(Pid1,{inputNames}),
        add:rpc(Pid1,{outputNames}),
        add:rpc(Pid1,{calculate,"OUTPUT",0}).
	