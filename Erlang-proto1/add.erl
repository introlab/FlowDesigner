-module(add).
-compile(export_all).

start() ->spawn(fun add_loop/0).

rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
			Response
	end.
	
add_list([H|T]) -> H + add_list(T);
add_list([]) -> 0.

sum_two(A,B) -> A + B.	
	
add_loop() ->
	receive
	{From, {calculate, Count, Val1, Val2} } ->
		From ! {self(), {ok, Count, Val1 + Val2} },
		add_loop();
	{From, {calculate, Count, L } } ->
		From ! {self(), {ok, Count, add_list(L)}},
		add_loop();	
	{From, Other} ->
		From ! {self(), {error, Other} } ,
		add_loop()
	end.
	
test() ->
	Pid = add:start(),
	add:rpc(Pid, {calculate,1,2,3}).
	