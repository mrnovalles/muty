-module(lock1).
-export([init/2]).

init(_, Nodes) ->
	open(Nodes).

open(Nodes) ->
	receive
		{take, Master} ->	%my worker wants the lock. not any other worker
			Refs = requests(Nodes),	% inform all other locks that i need to be taken
			wait(Nodes, Master, Refs, []);	%and enter waiting state
		{request, From, Ref} ->	%request from another lock
			From ! {ok, Ref},	%immediately reply ok (i'm in open state) 
			open(Nodes);	%and enter open state again
		stop ->
			ok
	end.

requests(Nodes) ->	%send a request message to all locks
	lists:map(fun(P) -> R = make_ref(), P ! {request, self(), R}, R end, Nodes).

wait(Nodes, Master, [], Waiting) ->	%waiting state with empty list of locks -- all of the locks sent me an ok message!! -> I can take the lock!
	Master ! taken,		%the lock is taken
	held(Nodes, Waiting);	%enter the held state

wait(Nodes, Master, Refs, Waiting) ->	%waiting for ok messages. Master is my worker. Refs is the list of nodes waiting to delete from after getting an ok, and Waiting is the list of nodes I am waiting to get the ok from.
	receive
		{request, From, Ref} ->	%another lock sent me a request message -> I keep it in a list(Waiting) to sent back an ok message
			wait(Nodes, Master, Refs, [{From, Ref}|Waiting]);	%when I release my lock
		{ok, Ref} ->	%i received an ok message from a lock
			Refs2 = lists:delete(Ref, Refs),	%and I delete it from my list
			wait(Nodes, Master, Refs2, Waiting);	%waiting for the rest of the ok messages
		release ->	%I have to relesae the lock
			ok(Waiting),	%I sent ok messages to the locks that requested me while I was waiting
			open(Nodes)	%back to open state
	end.

ok(Waiting) ->	%send ok message
	lists:map(fun({F,R}) -> F ! {ok, R} end, Waiting).

held(Nodes, Waiting) ->	%The lock is mine!!!
	receive
		{request, From, Ref} ->	%I keep on accepting requests from other locks to send them ok afterwards
			held(Nodes, [{From, Ref}|Waiting]);
		release ->	%release message from the worker -> I have to release the lock
			ok(Waiting),	%I inform the waiting locks!
			open(Nodes)	%back to open state!!
	end.

