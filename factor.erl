-module(factor).

-export([biggest_prime_factor/1]).
-export([test/0]).


-spec biggest_prime_factor(integer()) -> integer().
biggest_prime_factor(N) ->
	biggest_prime_factor(2, N).

biggest_prime_factor(X, N) when X == N ->
	X;
biggest_prime_factor(X, N) ->
	case N rem X of
		0 ->
			biggest_prime_factor(X, N div X);
		_ ->
			biggest_prime_factor(X + 1, N)
	end.


test() ->
	biggest_prime_factor(600851475143).
