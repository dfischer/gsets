-module(uod).

-export([
         new/0, new/1,
         add_term/2,
         get_dterm/2,
         to_int/1, to_dterm/1
        ]).

-export_type([uod/0, dterm/0]).

-record(uod,
        {
          terms = #{} :: #{term() => dterm()},
          latest = 2 :: non_neg_integer()
        }).

new() ->
    #uod{}.

new(#uod{} = UoD) ->
    UoD.

-opaque uod() :: #uod{}.
-opaque dterm() :: pos_integer().

-spec add_term(term(), uod()) -> {dterm(), uod()}.
add_term(Term, #uod{terms = Terms,
                    latest = Latest} = UoD) ->
    case maps:get(Term, Terms, undefined) of
        undefined ->
            Next = primes:next_prime(Latest),
            %% Terms1 = Terms#{Term => Latest},
            Terms1 = maps:put(Term, Latest, Terms),
            {Latest, UoD#uod{terms = Terms1, latest = Next}};
        Prime->
            {Prime, UoD}
    end.

-spec get_dterm(term(), uod()) -> dterm() | undefined.
get_dterm(Term, #uod{terms = Terms}) ->
    maps:get(Term, Terms, undefined).

-spec to_int(dterm()) -> pos_integer().
to_int(Dterm) ->
    Dterm.

-spec to_dterm(pos_integer()) -> dterm().
to_dterm(Int) ->
    Int.
