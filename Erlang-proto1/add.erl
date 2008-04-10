-module(add).
-compile(export_all).

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

make_output(Count, Name, Value) ->
    {Name,Count,Value}.


make_input(NodeName, InputName) ->
    %We should look in a global repository the
    %PID associated with the node, since each node have
    %a unique PID.
    Pid = 0,
    {NodeName, InputName, Pid}.


generic_prefetch(Name,Count,[H|T]) ->   
    %Look for the adequate pattern in head
    case H of   
       {Count,Name,_} -> H;  
       {_,_,_} -> generic_prefetch(Name,Count,T)
    end;
    
 generic_prefetch(_,_,[]) -> {notFound}.   


generic_get_input(Name,Count,Pid) ->
    
    %Send calculate message
    Pid ! {self(), {calculate,Name,Count}},
    
    %Wait for the answer
    %Question : Can we get other messages during this time ???
    %We suppose that is the case...
    receive
        {Pid, Answer} ->
            case Answer of
                {ok, {Name,Count,Data} }-> [{Name,Count,Data}];

                %This should throw an exception
                {error, _} -> []

            end;
        
        %Other messages should be put back to our message Queue?
        Other -> self() ! Other,
        
        %We should wait for the answer
        generic_get_input(Name,Count,Pid)
        
        
        %Add a timer to return to main loop if
        %taking too much time
    end.
    
 generic_get_all_inputs([InputsH|InputsT]) ->   
    %Get the input fields 
    {Name,Count,Pid} = InputsH,
    Result = generic_get_input(Name,Count,Pid),     
    [Result] ++ generic_get_all_inputs(InputsT);
 generic_get_all_inputs([]) -> [].
	
add_loop(Inputs, Outputs, Parameters) ->
	receive
        
        %Real Calculate will fetch inputs 
        %From another process (node)
        {From, {calculate, OutputName, Count}} ->
            
            %Already calculated?
            PreFetch = generic_prefetch("OUTPUT",Count,Outputs),
        
            %output Prefetch for debug
            io:format("+++debug+++ Prefetch = ~p~n",[PreFetch]),
            
            case PreFetch of
                    {notFound} -> 
                
                        %Get our inputs
                        L = generic_get_all_inputs(Inputs),
        
                        %Store value 
                        Result = make_output(Count,"OUTPUT",add_list(L)),
                
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
                PreFetch = generic_prefetch("OUTPUT",Count,Outputs),
                               
                %output Prefetch for debug
                io:format("+++debug+++ Prefetch = ~p~n",[PreFetch]),
                
                case PreFetch of
                    {notFound} -> 
                
                        %Get our inputs
        
                        %Store value 
                        Result = make_output(Count,"OUTPUT",Val1 + Val2),
                
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
	