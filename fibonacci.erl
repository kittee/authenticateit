-module(fibonacci).

-export([count_even/1]).
-export([test/0]).


-spec count_even(integer()) -> integer().
count_even(Limit) ->
	count_even(0, 1, 0, Limit).

count_even(_X, Y, Sum, Limit) when Y >= Limit ->
	Sum;
count_even(X, Y, Sum, Limit) when Y rem 2 =:= 0 ->
	count_even(Y, X + Y, Sum + Y, Limit);
count_even(X, Y, Sum, Limit) ->
	count_even(Y, X + Y, Sum, Limit).


test() ->
	count_even(4000000).
