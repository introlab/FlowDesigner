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
     
 
 generic_node_message_handler(NodeName, InputNames, OutputNames, Inputs, Outputs, Parameters) ->
 

    receive
       
        %Real Calculate will fetch inputs 
        %From another process (node)        
        {From, {calculate, OutputName, Count}} ->

            %Already calculated?
            PreFetch = dataFlow:generic_prefetch(OutputName,Count,Outputs),

            %output Prefetch for debug
            io:format("+++debug+++ Prefetch = ~p~p~n",[NodeName,PreFetch]),

            %Looking for already calculated values
            case PreFetch of
            {notFound} -> 
                %Send to ourself the request to calculate
                self() ! {From, {calculateNode, OutputName, Count}},
                %Return new state
                {NodeName, InputNames, OutputNames, Inputs, Outputs, Parameters};

             %Valid data prefetch send answer
             {_,_,_} -> 
                NewOutputs = Outputs, 
                self() ! {From, {handled, {ok, PreFetch} }},
                %Return new state
                {NodeName, InputNames, OutputNames, Inputs, Outputs, Parameters}
            end;
                      
        %Input Name request    
        {From, {inputNames}} ->
                self() ! {From, {handled, {inputNames, InputNames}}},
                %Return new state
                {NodeName, InputNames, OutputNames, Inputs, Outputs, Parameters};
            
        %Output names request
        {From, {outputNames}} ->
                self() ! {From, {handled, {outputNames, OutputNames}}},
                %Return new state
                {NodeName, InputNames, OutputNames, Inputs, Outputs, Parameters};
                                  
        {From, {inputs}} ->
                self() ! {From, {handled, {inputs, Inputs}}},
                %Return new state
                {NodeName, InputNames, OutputNames, Inputs, Outputs, Parameters};
                
        {From, {outputs}} ->
                self() ! {From, {handled, {outputs, Outputs}}},
                %Return new state
                {NodeName, InputNames, OutputNames, Inputs, Outputs, Parameters};
                                           
        {From, {parameters}} ->
                self() ! {From, {handled, {parameters, Parameters}}},
                %Return new state
                {NodeName, InputNames, OutputNames, Inputs, Outputs, Parameters};
                
        {From, {connect, NodeName, OutputName, InputName,Pid}} ->
                NewInputs = Inputs ++ [{NodeName,OutputName,InputName,Pid}],
                self() ! {From, {handled, {connect, ok}}},
                %Return new state
                {NodeName, InputNames, OutputNames, NewInputs, Outputs, Parameters};
                
        
        %Other messages will be handled by the
        %receive in the calling process
        %This will prevent to block
        Message ->
                io:format("+++Forwarding : ~p~n",[Message]),
		self() ! Message       
     end.
 
     
 