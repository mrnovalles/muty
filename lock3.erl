-module(lock2).
-export([init/3]).

%lock3:

init(MyId, Nodes,MyClock) ->	%when the lock is started it is given a unique ID: MyId
	MyClock = 0,
	open(Nodes, MyId, MyClock).
		

open(Nodes, MyId, MyClock) ->
	receive
		{take, Master} ->	%my worker wants the lock
			Refs = requests(Nodes, MyId, MyClock),	%inform all other locks that i need to be taken
			wait(Nodes, Master, Refs, [], MyId, MyClock, MyClock);	%and enter waiting state MyClock twice, 1 will change the other not
		{request, From, Ref, _, Clock} ->	%request from another lock
			NewClock = lists:max([MyClock,Clock]) + 1, 
			From ! {ok, Ref, NewClock},	%immediately reply ok (i'm in open state) 
			open(Nodes, MyId, NewClock);	%and enter open state again
		stop ->
			ok
	end.

requests(Nodes, MyId, MyClock) ->	%send a request message to all locks
	lists:map(fun(P) -> R = make_ref(), P ! {request, self(), R, MyId, MyClock}, R end, Nodes).	%I send my ID together with the request message.

wait(Nodes, Master, [], Waiting, MyId,MyClock, MyReqClock) ->	%waiting state with empty list of locks -- all of the locks sent me an ok message!! -> I can take the lock!
	Master ! taken,		%the lock is taken
	held(Nodes, Waiting, MyId, MyClock);	%enter the held state

wait(Nodes, Master, Refs, Waiting, MyId, MyClock, MyReqClock) ->	%waiting for ok messages
	receive
	%Req msg now haas MyId and also MyClock to compare to it.
		{request, From, Ref, Req_Id, Req_Clock} ->
			NewClock = lists:max([MyClock,Clock]) + 1, %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			if 
				MyReqClock > Req_Clock ->
				From ! {ok, Ref, NewClock} %we send the New Clock Value value to the other process to update its clock			
				
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
