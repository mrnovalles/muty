-module(lock2).
-export([init/2]).

%lock2: Each lock has a unique identifier: 1,2,3,4 which gives the lock a priority.
%change the waiting state so that a lock in the waiting state will send ok to a request
%if this request is coming from a lock with higher priority. If it comes from a lock
%with lower priority, then proceed as in lock1.

init(MyId, Nodes) ->	%when the lock is started it is given a unique ID: MyId
	open(Nodes, MyId).
	

open(Nodes, MyId) ->
	receive
		{take, Master} ->	%my worker wants the lock
			Refs = requests(Nodes, MyId),	%inform all other locks that i need to be taken
			wait(Nodes, Master, Refs, [], MyId);	%and enter waiting state
		{request, From, Ref, _} ->	%request from another lock
			From ! {ok, Ref},	%immediately reply ok (i'm in open state) 
			open(Nodes, MyId);	%and enter open state again
		stop ->
			ok
	end.

requests(Nodes, MyId) ->	%send a request message to all locks
	lists:map(fun(P) -> R = make_ref(), P ! {request, self(), R, MyId}, R end, Nodes).	%I send my ID together with the request message.

wait(Nodes, Master, [], Waiting, MyId) ->	%waiting state with empty list of locks -- all of the locks sent me an ok message!! -> I can take the lock!
	Master ! taken,		%the lock is taken
	held(Nodes, Waiting, MyId);	%enter the held state

wait(Nodes, Master, Refs, Waiting, MyId) ->	%waiting for ok messages
	receive
	%prepei na allaksw to format tou mhnymatos request, wste na periexei kai to Id tou lock apo to opoio erxetai to request!!!
		{request, From, Ref, Req_Id} ->
			if
				MyId > Req_Id ->	%The requesting lock has higher priority
				From ! {ok, Ref},	%send an ok message to it!	
				%send another request to the lock with higher priority
				%to ensure the "happened before" order
				Ref2 = requests([From], MyId),
				wait(Nodes, Master, Ref2, Waiting, MyId);	%and enter waiting state
				% NOT back to open state -> back to WAIT state!!!
			
				true ->	%I have higher priority: I keep the request and I go on :p
				wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyId)
			end;
		{ok, Ref} ->	%i received an ok message from a lock
			Refs2 = lists:delete(Ref, Refs),	%and I delete it from my list
			wait(Nodes, Master, Refs2, Waiting, MyId);	%waiting for the rest of the ok messages
		release ->	%I have to release the lock
			ok(Waiting),	%I sent ok messages to the locks that requested me while I was waiting
			open(Nodes, MyId)	%back to open state
	end.

ok(Waiting) ->	%send ok message
	lists:map(fun({F,R}) -> F ! {ok, R} end, Waiting).

held(Nodes, Waiting, MyId) ->	%The lock is mine!!!
	receive
		{request, From, Ref, _} ->	%I keep on accepting requests from other locks to send them ok afterwards
			held(Nodes, [{From, Ref}|Waiting], MyId);
		release ->	%release message from the worker -> I have to release the lock
			ok(Waiting),	%I inform the waiting locks!
			open(Nodes, MyId)	%back to open state!!
	end.
