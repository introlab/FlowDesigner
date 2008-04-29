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
	receive
        
        %Real Calculate will fetch inputs 
        %From another process (node)
        {From, {calculate, OutputName, Count}} ->
            
            %Already calculated?
            PreFetch = dataFlow:generic_prefetch(OutputName,Count,Outputs),
        
            %output Prefetch for debug
            io:format("+++debug+++ Prefetch = ~p~n",[PreFetch]),
            
            case PreFetch of
                    {notFound} -> 
                
                        %Get our inputs
                        L = dataFlow:generic_get_all_inputs(Inputs),
        
                        %Store value 
                        Result = dataFlow:generic_make_output(Count,OutputName,add_list(L)),
                
                        %Put it in the Output Buffer
                        %Buffer should be managed according to 
                        %N lookback / lookahead values
                        NewOutputs = Outputs ++ [Result],
        
                        From ! {self(), {ok, Result} };
                        
                     {_,_,_} -> 
                        NewOutputs = Outputs, 
                        From ! {self(), {ok, PreFetch} }
                end,
                        
                add_loop(NodeName,InputNames,OutputNames,Inputs,NewOutputs,Parameters);
               
               
        {From, {inputNames}} ->
                From ! {self(), {inputNames, InputNames}},
                add_loop(NodeName,InputNames,OutputNames,Inputs,Outputs,Parameters);       
        
        {From, {outputNames}} ->
                From ! {self(), {outputNames, OutputNames}},
                add_loop(NodeName,InputNames,OutputNames,Inputs,Outputs,Parameters);         
                           
        {From, {inputs}} ->
                From ! {self(), {inputs, Inputs}},
                add_loop(NodeName,InputNames,OutputNames,Inputs,Outputs,Parameters);
        
        {From, {outputs}} ->
                From ! {self(), {outputs, Outputs}},
                add_loop(NodeName,InputNames, OutputNames, Inputs,Outputs,Parameters);
                
        {From, {parameters}} ->
                From ! {self(), {parameters, Parameters}},
                add_loop(NodeName,InputNames, OutputNames, Inputs,Outputs,Parameters); 
                
        {From, {connect, NodeName, OutputName, InputName,Pid}} ->
                NewInputs = Inputs ++ [{NodeName,OutputName,InputName,Pid}],
                From ! {self(), {connect, ok}},
                add_loop(NodeName,InputNames, OutputNames, NewInputs,Outputs,Parameters);
                
        {From, Other} ->
		From ! {self(), {error, Other} } ,
		add_loop(NodeName,InputNames, OutputNames, Inputs,Outputs,Parameters)
	end.
	
test() ->
	{Pid1,State1} = add:start(add1,[],[],[]), 
        io:format("+++Node Started+++ Info = ~p~p~n",[Pid1,State1]),
        {Pid2, State2} = add:start(add2,[],[],[]),
        io:format("+++Node Started+++ Info = ~p~p~n",[Pid2,State2]),
        add:rpc(whereis(add1),{inputNames}).
        
 
	