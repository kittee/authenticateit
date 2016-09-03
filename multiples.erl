-module(multiples).

-export([count_3_5/1]).
-export([test/0]).

-spec count_3_5(integer()) -> integer().
count_3_5(Limit) ->
	count_3_5(0, 0, 0, Limit).

count_3_5(P3, P5, Sum, Limit) when P3 >= Limit, P5 >= Limit ->
	Sum;
count_3_5(P3, P5, Sum, Limit) when P3 =:= P5 ->
	count_3_5(P3 + 3, P5 + 5, Sum + P3, Limit);
count_3_5(P3, P5, Sum, Limit) when P3 < P5 ->
	count_3_5(P3 + 3, P5, Sum + P3, Limit);
count_3_5(P3, P5, Sum, Limit) when P3 > P5 ->
	count_3_5(P3, P5 + 5, Sum + P5, Limit).


test() ->
	count_3_5(1000).
