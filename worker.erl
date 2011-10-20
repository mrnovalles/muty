-module(worker).
-export([init/5]).
-define(deadlock, 4000).

init(Name,Lock,Seed,Sleep,Work) ->
	Gui = spawn(gui, init, [Name]),
	random: seed(Seed, Seed, Seed),
	Taken =  worker(Name, Lock, [], Sleep, Work, Gui),
	Gui ! stop,
	terminate(Name, Taken).
	
worker(Name, Lock, Taken, Sleep, Work, Gui)->
	Wait = random:uniform(Sleep),
	receive
		stop ->
			Taken
	after 	Wait ->
			T=critical(Name,Lock,Work,Gui), % T could be the atom no or {taken,T} being T the seconds it waited to get it.
			worker(Name,Lock,[T|Taken], Sleep,Work,Gui)
	end.
	
critical(Name, Lock, Work, Gui) ->
	T1 = now(),
	Gui ! waiting,
	Lock ! {take, self()},
	receive
		taken  ->
			T = elapsed(T1),
			io:format("~w: lock taken in ~w ms ~n",[Name,T]),
			Gui ! taken,
			timer:sleep(random:uniform(Work)),
			Gui ! leave,
			Lock ! release,
			{taken, T}
	after ?deadlock ->
		io:format("~w: giving up ~n", [Name]),
		Lock ! release,
		Gui ! leave,
		no
	end.
	
elapsed({_,S1,M1}) ->
		{_,S2,M2} = now(),
		(S2 - S1)*1000 + ((M2 - M1) div 1000).

terminate(Name, Taken) ->
	{Locks, Time, Dead} =
		lists:foldl(
			fun(Entry,{L,T,D}) ->
				case Entry of
					{taken,I} ->
						{L+1,T+I,D};
					_ ->
						{L,T,D+1}
				end
			end,
			{0,0,0}, Taken),
	if
		Locks > 0 ->
			Average = Time / Locks;
		true ->
			Average = 0
	end,
	io:format("~s: ~w locks taken, average of ~w ms, ~w deadlock situations~n",
		[Name, Locks, Average, Dead]).

