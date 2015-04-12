-module(gset).

-export([new/0, is_set/1, size/1, to_list/1, from_list/1]).
-export([is_element/2, add_element/2, del_element/2]).
-export([is_disjoint/2]).
-export([union/2, union/1, intersection/2, intersection/1]).
-export([subtract/2, is_subset/2]).
-export([fold/3, filter/2]).

-record(gset,
        {
          %% while we might know the size of sets we've added, it
          %% isn't clear how to track sizes of unions and
          %% intersections, so we just omit the size.
          set = 1 :: pos_integer()
        }).

-opaque gset() :: #gset{}.
-export_type([gset/0]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-spec new() -> gset().
new() ->
    #gset{}.

-spec is_set(term()) -> boolean().
is_set(#gset{}) ->
    true;
is_set(_) ->
    false.

%% this cannot be supported without factoring, which is rather
%% expensive (intractably so for large sets), so we punt.
-spec size(gset()) -> unknown.
size(_) ->
    unknown.

%% see size/1 for the reasons we cannot do this.
-spec to_list(gset()) -> no.
to_list(_) ->
    no.

%% see size/1
-spec fold(_, _, _) -> no.
fold(_, _, _) ->
    no.

%% see size/1
-spec filter(_, _) -> no.
filter(_, _) ->
    no.

-spec from_list([term()]) -> gset().
from_list(List) ->
    from_list_int(List, new()).

from_list_int([], Acc) ->
    Acc;
from_list_int([H|List], Acc) ->
    Acc1 = add_element(H, Acc),
    from_list_int(List, Acc1).

-spec add_element(uod:dterm(), gset()) -> gset().
add_element(Elt0, #gset{set = Set} = Gset) ->
    case is_element(Elt0, Gset) of
        true ->
            Gset;
        _ ->
            Elt = uod:to_int(Elt0),
            #gset{set = Set * Elt}
    end.

-spec is_element(uod:dterm(), gset()) -> boolean().
is_element(Elt0, #gset{set = Set}) ->
    Elt = uod:to_int(Elt0),
    case Set rem Elt of
        0 -> true;
        _ -> false
    end.

-spec del_element(uod:dterm(), gset()) -> gset().
del_element(_Elt, #gset{set = 1} = Gset) ->
    Gset;
del_element(Elt0, #gset{set = Set} = Gset) ->
    case is_element(Elt0, Gset) of
        true ->
            Elt = uod:to_int(Elt0),
            Gset#gset{set = Set div Elt};
        _ ->
            Gset
    end.

-spec union(gset(), gset()) -> gset().
union(#gset{set = A}, #gset{set = B}) ->
    #gset{set = lcm(A, B)}.

-spec union([gset()]) -> gset().
union(List) ->
    lists:foldl(fun(X, Acc) ->
                        union(X, Acc)
                end,
                #gset{},
                List).

-spec intersection(gset(), gset()) -> gset().
intersection(#gset{set = A}, #gset{set = B}) ->
    Set = gcd(A, B),
    case Set of
        1 ->
            #gset{};
        _ ->
            #gset{set = Set}
    end.

-spec intersection([gset()]) -> gset().
intersection(List) ->
    lists:foldl(fun(X, Acc) ->
                        intersection(X, Acc)
                end,
                #gset{},
                List).

-spec subtract(gset(), gset()) -> gset().
subtract(#gset{set = A}, #gset{set = B}) ->
    #gset{set = A div gcd(A, B)}.

-spec is_subset(gset(), gset()) -> boolean().
is_subset(#gset{set = A0}, B) ->
    A = uod:to_dterm(A0),
    is_element(A, B).

-spec is_disjoint(gset(), gset()) -> boolean().
is_disjoint(A, B) ->
    case intersection(A, B) of
        #gset{set = 1} ->
            true;
        _ ->
            false
    end.

%%% internal functions
gcd(A, B) when B > A ->
    gcd(B, A);
gcd(A, B) ->
    case A rem B of
        0 ->
            B;
        Rem ->
            gcd(B, Rem)
    end.

lcm(A, B) when B > A ->
    lcm(B, A);
lcm(A, B) ->
    (A div gcd(A, B)) * B.


%%%%%%%% TESTS %%%%%%%%%%%

-ifdef(TEST).

basic_test() ->
    UoD = uod:new(),
    {A, UoD1} = uod:add_term(a, UoD),
    {B, UoD2} = uod:add_term(b, UoD1),
    {C, UoD3} = uod:add_term(c, UoD2),
    {D, _UoD4} = uod:add_term(d, UoD3),

    Set = new(),
    SetA = add_element(A, Set),
    SetA1 = add_element(B, SetA),
    SetB = add_element(B, Set),
    SetB1 = add_element(C, SetB),
    SetB2 = add_element(D, SetB1),
    ?assert(is_element(A, SetA1)),
    AIB = intersection(SetA1, SetB2),
    ?assert(is_element(B, AIB)),
    ?assert(false == is_element(A, AIB)).

basic_props_test() ->
    ?assert(proper:quickcheck(gset_props:statem())).

-endif.
