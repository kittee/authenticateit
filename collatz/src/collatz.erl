-module(collatz).

-export([count/2]).
-export([test/0]).

count(Start, Stop) ->
	application:ensure_all_started(collatz),
	collatz_server:count(Start, Stop).

test() ->
	count(1,1000000).