-module(lock2).
-export([init/2]).

init(Priority, Nodes) ->
	open(Priority,Nodes).

open(Priority,Nodes) ->
	receive
		{take, Master} ->	%my worker wants the lock. not any other worker
			Refs = requests(Priority,Nodes),	% inform all other locks that i need to be taken
			wait(Nodes , Priority, Master, Refs, []);	%and enter waiting state
		{request,_, From, Ref} ->	%request from another lock
			From ! {ok, Ref},	%immediately reply ok (i'm in open state) 
			open(Priority,Nodes);	%and enter open state again
		stop ->
			ok
	end.

requests(Priority,Nodes) ->	%send a request message to all locks
	lists:map(fun(P) -> R = make_ref(), P ! {request, Priority ,self(), R}, R end, Nodes).

wait(Nodes,Priority, Master, [], Waiting) ->	%waiting state with empty list of locks -- all of the locks sent me an ok message!! -> I can take the lock!
	Master ! taken,		%the lock is taken
	held(Priority,Nodes, Waiting);	%enter the held state

wait(Nodes,MyPriority, Master, Refs, Waiting) ->	%waiting for ok messages. Master is my worker. Refs is the list of nodes waiting to delete from after getting an ok, and Waiting is the list of nodes I am waiting to get the ok from.
	receive
		{request,Priority,From, Ref} ->	%another lock sent me a request message.I keep it in a list(Waiting) 2 sent back an ok msg 
						% Ref is the answer to a unique request
		if
			MyPriority > Priority -> %if the request has a higher priority
				From ! { ok, Ref},
				Refs = requests(MyPriority, From), %only requesting to From? 
				wait(MyPriority,Nodes, Master, Refs, Waiting);
				% wait here instead of Open
				%another request is sent so thar it can assure that there aren't 2 processes inside the critical zone at any time
		true->
			wait(Nodes, MyPriority, Master, Refs, [{From, Ref}|Waiting])	%I add a new node to Waiting
		end;
				
		{ok, Ref} ->	%i received an ok message from a lock
			Refs2 = lists:delete(Ref, Refs),	%and I delete it from my list
			wait(MyPriority,Nodes, Master, Refs2, Waiting);	%waiting for the rest of the ok messages
		release ->	%I have to relesae the lock
			ok(Waiting),	%I sent ok messages to the locks that requested me while I was waiting
			open(MyPriority ,Nodes)	%back to open state
	end.

ok(Waiting) ->	%send ok message
	lists:map(fun({F,R}) -> F ! {ok, R} end, Waiting).

held(Priority,Nodes, Waiting) ->	%The lock is mine!!!
	receive
		{request,Priority, From, Ref} ->	%I keep on accepting requests from other locks to send them ok afterwards
			held(Priority, Nodes, [{From, Ref}|Waiting]);
		release ->	%release message from the worker -> I have to release the lock
			ok(Waiting),	%I inform the waiting locks!
			open(Priority,Nodes)	%back to open state!!
	end.
