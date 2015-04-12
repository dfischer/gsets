-module(primes).

-export([primes/1, next_prime/1]).

is_prime(2) ->
    true;
is_prime(3) ->
    true;
is_prime(N) ->
    MaxTry = trunc(math:sqrt(N)),
    is_prime(N, 3, MaxTry).

is_prime(N, Try, MaxTry) ->
    if
        Try > MaxTry ->
            true;
        true ->
            if
                N rem Try == 0 ->
                    false;
                true ->
                    is_prime(N, Try+2, MaxTry)
            end
    end.

next_prime(2) ->
    3;
next_prime(Current) ->
    Next = Current + 2,
    case is_prime(Next) of
        true ->
            Next;
        false ->
            next_prime(Next)
    end.

acc_primes(Max, Current, Acc) ->
    Next = next_prime(Current),
    case Next >= Max of
        true -> lists:reverse(Acc);
        false -> acc_primes(Max, Next, [Next|Acc])
    end.

primes(N) ->
    acc_primes(N, 3, [3, 2]).
