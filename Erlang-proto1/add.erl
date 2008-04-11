-module(add).
-compile(export_all).
-include("dataFlow.hrl").

start(Inputs,Outputs,Parameters) ->
    spawn(fun() -> add_loop(Inputs, Outputs, Parameters) end).

rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
			Response
	end.
	
add_list([H|T]) -> H + add_list(T);
add_list([]) -> 0.





	
add_loop(Inputs, Outputs, Parameters) ->
	receive
        
        %Real Calculate will fetch inputs 
        %From another process (node)
        {From, {calculate, OutputName, Count}} ->
            
            %Already calculated?
            PreFetch = dataFlow:generic_prefetch("OUTPUT",Count,Outputs),
        
            %output Prefetch for debug
            io:format("+++debug+++ Prefetch = ~p~n",[PreFetch]),
            
            case PreFetch of
                    {notFound} -> 
                
                        %Get our inputs
                        L = dataFlow:generic_get_all_inputs(Inputs),
        
                        %Store value 
                        Result = dataFlow:make_output(Count,"OUTPUT",add_list(L)),
                
                        %Put it in the Output Buffer
                        %Buffer should be managed according to 
                        %N lookback / lookahead values
                        NewOutputs = Outputs ++ [Result],
        
                        From ! {self(), {ok, Result} };
                        
                     {_,_,_} -> 
                        NewOutputs = Outputs, 
                        From ! {self(), {ok, PreFetch} }
                end,
                        
                add_loop(Inputs,NewOutputs,Parameters);
            
            
	{From, {calculate, Count, Val1, Val2} } ->
            
                %Verify if we have already calculated this
                %iteration
                PreFetch = dataFlow:generic_prefetch("OUTPUT",Count,Outputs),
                               
                %output Prefetch for debug
                io:format("+++debug+++ Prefetch = ~p~n",[PreFetch]),
                
                case PreFetch of
                    {notFound} -> 
                
                        %Get our inputs
        
                        %Store value 
                        Result = dataFlow:make_output(Count,"OUTPUT",Val1 + Val2),
                
                        %Put it in the Output Buffer
                        %Buffer should be managed according to 
                        %N lookback / lookahead values
                        NewOutputs = Outputs ++ [Result],
        
                        From ! {self(), {ok, Result} };
                        
                     {_,_,_} -> 
                        NewOutputs = Outputs, 
                        From ! {self(), {ok, PreFetch} }
                end,
                        
                add_loop(Inputs,NewOutputs,Parameters);
                       
        {From, {inputs}} ->
                From ! {self(), {inputs, Inputs}},
                add_loop(Inputs,Outputs,Parameters);
        
        {From, {outputs}} ->
                From ! {self(), {outputs, Outputs}},
                add_loop(Inputs,Outputs,Parameters);
                
        {From, {parameters}} ->
                From ! {self(), {parameters, Parameters}},
                add_loop(Inputs,Outputs,Parameters); 
                
        {From, {connect, NodeName, OutputName, InputName,Pid}} ->
                NewInputs = Inputs ++ [{NodeName,OutputName,InputName,Pid}],
                From ! {self(), {connect, ok}},
                add_loop(NewInputs,Outputs,Parameters);
                
        {From, Other} ->
		From ! {self(), {error, Other} } ,
		add_loop(Inputs,Outputs,Parameters)
	end.
	
test() ->
	Pid = add:start([],[],[]),
    
        Pid2 = add:start([],[],[]),
    
        add:rpc(Pid, {connect,"TESTNODE","OUTPUT","INPUT",Pid2}),
    
    
	add:rpc(Pid, {calculate,0,2,3}),add:rpc(Pid, {inputs}), 
            add:rpc(Pid, {outputs}), 
            add:rpc(Pid, {parameters}),
        
        add:rpc(Pid, {calculate,1,2,3}),add:rpc(Pid, {inputs}), 
            add:rpc(Pid, {outputs}), 
            add:rpc(Pid, {parameters}),
        
        %Trying to get already calculated value
        add:rpc(Pid, {calculate,1,2,3}),add:rpc(Pid, {inputs}), 
            add:rpc(Pid, {outputs}), 
            add:rpc(Pid, {parameters}),
        
        {add:rpc(Pid, {calculate,2,2,3}),add:rpc(Pid, {inputs}), 
            add:rpc(Pid, {outputs}), 
            add:rpc(Pid, {parameters})}.
	