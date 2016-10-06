-module(uod).

-export([
         new/0, new/1,
         size/1,
         add_term/2,
         get_dterm/2,
         terms/1,
         to_int/1, to_dterm/1
        ]).

-record(uod,
        {
          terms = #{} :: #{term() => dterm()},
          latest = 2 :: non_neg_integer()
        }).

-opaque uod() :: #uod{}.
-opaque dterm() :: pos_integer().

-export_type([uod/0, dterm/0]).

-spec new() -> uod().
new() ->
    #uod{}.

-spec new(uod()) -> uod().
new(#uod{} = UoD) ->
    UoD.

-spec size(uod()) -> non_neg_integer().
size(UoD) ->
    maps:size(UoD#uod.terms).

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

-spec get_dterm(term(), uod()) -> {ok, dterm()} | {error, undefined}.
get_dterm(Term, #uod{terms = Terms}) ->
    case maps:get(Term, Terms, undefined) of
        undefined ->
            {error, undefined};
        DTerm ->
            {ok, DTerm}
    end.

-spec terms(uod()) -> [any()].
terms(#uod{terms = T}) ->
    maps:keys(T).

-spec to_int(dterm()) -> pos_integer().
to_int(Dterm) ->
    Dterm.

-spec to_dterm(pos_integer()) -> dterm().
to_dterm(Int) ->
    Int.
