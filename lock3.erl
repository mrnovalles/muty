-module(lock3).
-export([init/2]).

init(Priority, Nodes) ->
	Time = 0,
	open(Time, Priority,Nodes).

open(MyTime, Priority,Nodes) ->
	receive
		{take, Master} ->	%my worker wants the lock. not any other worker
			Refs = requests(Time, Priority, Nodes),	% inform all other locks that i need to be taken
			wait(Nodes , Time, Priority, Master, Refs, []);	%and enter waiting state
		{request,Time,_, From, Ref} ->	%request from another lock
			if
				Time > MyTime -> %check if recved time 
					MyTime = Time +1,
				true ->
					MyTime = MyTime +1,
			end;
					From ! {ok, Ref},	%immediately reply ok (i'm in open state) 
					open(MyTime, Priority,Nodes);	%and enter open state again
		stop ->
			ok
	end.

requests(Priority,Nodes) ->	%send a request message to all locks
	lists:map(fun(P) -> R = make_ref(), P ! {request, Time,Priority ,self(), R}, R end, Nodes).

wait(Nodes,MyTime, Priority, Master, [], Waiting) ->	%waiting state with empty list of locks -- all of the locks sent me an ok message!! -> I can take the lock!
	Master ! taken,		%the lock is taken
	held(MyTime, Priority,Nodes, Waiting);	%enter the held state

wait(Nodes, MyTime, MyPriority, Master, Refs, Waiting) ->	%waiting for ok messages. Master is my worker. Refs is the list of nodes waiting to delete from after getting an ok, and Waiting is the list of nodes I am waiting to get the ok from.
	receive
		{request,Time, Priority,From, Ref} ->	%another lock sent me a req message.I keep it in a list(Waiting) to sent back an ok msg //Ref is the answer to a unique request
		%TODO Check if the incoming req was sent before mine. If it cannot be determine check the Priority 
		if Time > MyTime ->
		%UPDATE MY TIME	
		true ->
		
		if
			MyPriority > Priority -> %if the request has a higher priority
				From ! { ok, Ref},
				Refs = requests(MyPriority, From), %only requesting another OK to From 
				%another request is sent so thar it can assure that there aren't 2 processes inside the critical zone at any time
				wait(MyPriority,Nodes, Master, Refs, Waiting); % wait again for the Ok to come here instead of Open
				
		true->
			wait(Nodes, MyPriority, Master, Refs, [{From, Ref}|Waiting])	%I add a new node to Waiting
		end;
				
		{ok, Ref} ->	%i received an ok message from a lock
			Refs2 = lists:delete(Ref, Refs),	%and I delete it from my list
			wait(MyPriority,Nodes, Master, Refs2, Waiting);	%waiting for the rest of the ok messages
		release ->	%I have to relesae the lock
			ok(Waiting),	%I sent ok messages to the locks that requested me while I was waiting
			open(MyPriority,Nodes)	%back to open state
	end.

ok(Waiting) ->	%send ok message
	lists:map(fun({F,R}) -> F ! {ok, R} end, Waiting).

held(Priority,Nodes, Waiting) ->	%The lock is mine!!!
	receive
		{request,Priority, From, Ref} ->	%I keep on accepting requests from other locks to send them ok afterwards
			held(Priority,Nodes, [{From, Ref}|Waiting]);
		release ->	%release message from the worker -> I have to release the lock
			ok(Waiting),	%I inform the waiting locks!
			open(Priority,Nodes)	%back to open state!!
	end.
