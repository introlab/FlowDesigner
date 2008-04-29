-module(dataFlow).
-compile(export_all).

generic_make_output(Count, Name, Value) ->
    {Name,Count,Value}.

generic_make_input(NodeName, InputName) ->
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