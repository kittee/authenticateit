-module(lcm).

-export([lcm/1]).
-export([test/0]).

-spec lcm([integer()]) -> integer().
lcm(Numbers) ->
	PrimeFactors = [prime_factors(N) || N <- Numbers],
	MaxExp = max_exp(PrimeFactors),
	multiply(MaxExp, 1).


prime_factors(N) ->
	prime_factors(2, N, []).

prime_factors(_X, 1, PFs) ->
	PFs;
prime_factors(X, N, PFs) ->
	case N rem X of
		0 ->
			prime_factors(X, N div X, power(X, PFs));
		_ ->
			prime_factors(X + 1, N, PFs)
	end.


power(B, [{B, E} | P]) ->
	[{B, E + 1} | P];
power(B, P) ->
	[{B, 1} | P].


max_exp(P) ->
	max_exp(lists:flatten(P), #{}).

max_exp([], M) ->
	maps:to_list(M);
max_exp([{B, E1} | P], M) ->
	case M of
		#{B := E2} when E1 > E2 ->
			max_exp(P, M#{B := E1});
		#{B := _E2} ->
			max_exp(P, M);
		#{} ->
			max_exp(P, M#{B => E1})
	end.


multiply([], Prod) ->
	Prod;
multiply([{_X, 0} | Rest], Prod) ->
	multiply(Rest, Prod);
multiply([{X, M} | Rest], Prod) ->
	multiply([{X, M - 1} | Rest], Prod * X).


test() ->
	lcm(lists:seq(1,20)).