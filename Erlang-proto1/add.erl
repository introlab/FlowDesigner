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
    {Count,Name,Value}.


generic_prefetch(Name,Count,[H|T]) ->   
    %Look for the adequate pattern in head
    case H of   
       {Count,Name,_} -> H;  
       {_,_,_} -> generic_prefetch(Name,Count,T)
    end;
    
 generic_prefetch(_,_,[]) -> {notFound}.   


	
add_loop(Inputs, Outputs, Parameters) ->
	receive
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
                
	{From, {calculate, Count, L } } ->
		From ! {self(), {ok, Count, add_list(L)}},
		add_loop(Inputs,Outputs,Parameters);	
        {From, {inputs}} ->
                From ! {self(), {inputs, Inputs}},
                add_loop(Inputs,Outputs,Parameters);
        {From, {outputs}} ->
                From ! {self(), {outputs, Outputs}},
                add_loop(Inputs,Outputs,Parameters);
        {From, {parameters}} ->
                From ! {self(), {parameters, Parameters}},
                add_loop(Inputs,Outputs,Parameters); 
        {From, Other} ->
		From ! {self(), {error, Other} } ,
		add_loop(Inputs,Outputs,Parameters)
	end.
	
test() ->
	Pid = add:start([],[],[]),
	add:rpc(Pid, {calculate,0,2,3}),add:rpc(Pid, {inputs}), 
            add:rpc(Pid, {outputs}), 
            add:rpc(Pid, {parameters}),
        
        add:rpc(Pid, {calculate,1,2,3}),add:rpc(Pid, {inputs}), 
            add:rpc(Pid, {outputs}), 
            add:rpc(Pid, {parameters}),
        
        add:rpc(Pid, {calculate,1,2,3}),add:rpc(Pid, {inputs}), 
            add:rpc(Pid, {outputs}), 
            add:rpc(Pid, {parameters}),
        
        {add:rpc(Pid, {calculate,2,2,3}),add:rpc(Pid, {inputs}), 
            add:rpc(Pid, {outputs}), 
            add:rpc(Pid, {parameters})}.
	