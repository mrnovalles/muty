-module(gui).
-export([start/1, init/1]).
start(Name) ->
spawn(gui, init, [Name]).
init(Name) ->
Width = 200,
      Height = 200,
      Win = gs:window(gs:start(),[{map,true},{title,Name},
		      {bg, blue}, {width,Width},{height,Height}]),
      loop(Win),
      gs:stop().
      loop(Win)->
      receive
      waiting ->
      gs:config(Win, [{bg, yellow}]),
      loop(Win);
taken ->
gs:config(Win, [{bg, red}]),
	loop(Win);
leave ->
gs:config(Win, [{bg, blue}]),
	loop(Win);
stop ->
ok;
Error ->
io:format("gui: strange message ~w ~n", [Error]),
	loop(Win)
	end.

