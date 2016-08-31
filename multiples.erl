-module(multiples).

-export([count_multiples/2]).
-export([test/0]).


-spec count_multiples([integer()], integer()) -> integer().
count_multiples(Multiples, Limit) ->
	lists:foldl(fun (X, Sum) -> Sum + count_multiples(X, 0, 0, Limit) end, 0, Multiples).

count_multiples(_X, Product, Sum, Limit) when Product >= Limit ->
	Sum;
count_multiples(X, Product, Sum, Limit) ->
	count_multiples(X, Product + X, Sum + Product, Limit).


test() ->
	count_multiples([3,5], 1000).
