-module(palindrome).

-export([max_palindrome_product/2]).
-export([test/0]).


-spec max_palindrome_product(integer(), integer()) -> integer() | not_found.
max_palindrome_product(Min, Max) ->
	max_palindrome_product(Min, Min, 0, Min, Max).

max_palindrome_product(_X, Y, 0, _Min, Max) when Y > Max ->
	not_found;
max_palindrome_product(_X, Y, Palindrome, _Min, Max) when Y > Max ->
	Palindrome;
max_palindrome_product(X, Y, Palindrome, Min, Max) when X > Max ->
	max_palindrome_product(Min, Y + 1, Palindrome, Min, Max);
max_palindrome_product(X, Y, Palindrome, Min, Max) ->
	NewPalindrome = case mirror(Candidate = X * Y, 10) of
		Candidate ->
			max(Candidate, Palindrome);
		_ ->
			Palindrome
	end,
	max_palindrome_product(X + 1, Y, NewPalindrome, Min, Max).


mirror(N, Base) ->
	mirror(N, 0, Base).

mirror(0, Rev, _Base) ->
	Rev;
mirror(N, Rev, Base) ->
	mirror(N div Base, Rev * Base + N rem Base, Base).


test() ->
	max_palindrome_product(100, 999).