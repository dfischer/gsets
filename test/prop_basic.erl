-module(prop_basic).

-include_lib("proper/include/proper.hrl").

-behaviour(proper_statem).

-export([
         command/1,
         initial_state/0,
         next_state/3,
         postcondition/3,
         precondition/2
        ]).

-export([prop_test/0]).

-record(s,
        {
          uod = uod:new() :: uod:uod(),
          set = sets:new() :: sets:set()
        }).

prop_test() ->
    ?FORALL(
       Cmds, commands(?MODULE),
       ?TRAPEXIT(
          begin
              put(mark_time, undefined),
              mark("pre"),
              gsets_srv:start_link(),
              {History, State, Result} = run_commands(?MODULE, Cmds),
              gsets_srv:stop(),
              mark("serv stop"),
              ?WHENFAIL(io:fwrite(user,
                                  "\nHistory: ~w\n"
                                  "State: ~w\n"
                                  "Result: ~w\n",
                                  [History,
                                   State,
                                   Result]),
                        aggregate(command_names(Cmds), Result =:= ok))
          end)).

%% generators

-type value() :: term().
value() ->
    atom().

make_set(Elements) ->
    %% io:fwrite(standard_error, "elements ~p~n", [Elements]),
    take_subset(Elements).

take_subset([]) ->
    error(badarg);
take_subset(L) ->
    Len = length(L),
    case Len of
        0 -> L;
        _ ->
            Take = random:uniform(Len),
            T = tks(L, Take, []),
            case T of
                [] -> L;
                _ -> T
            end
    end.

tks(_L, 0, Acc) ->
    Acc;
tks([], _, Acc) ->
    Acc;
tks([H|T], Take, Acc) ->
    case rand:uniform(2) of
        1 ->
            tks(T, Take - 1, [H|Acc]);
        _ ->
            tks(T, Take, Acc)
    end.

take_random([]) ->
    error(take_from_empty);
take_random(List) ->
    L = length(List),
    N = random:uniform(L),
    lists:nth(N, List).

mark() ->
    mark("mark").

mark(Marker) ->
    case get(mark_time) of
        undefined ->
            ok;
        Then ->
            Diff = timer:now_diff(now(), Then),
            %% io:fwrite(user, "~p ~p ~p~n", [Marker, Diff div 1, self()]),
            ok
    end,
    put(mark_time, now()).


%% model stuff

initial_state() ->
    mark("init"),
    random:seed(now()),
    #s{}.

command(S) ->
    frequency(
      [
       {10, {call, gsets_srv, add_element, [value()]}},
       {5, {call, gsets_srv, del_element, [value()]}}
      ]
      ++ [{2, {call, gsets_srv, add_element, [take_random(uod:terms(S#s.uod))]}}
          || uod:terms(S#s.uod) /= []]
      ++ [{2, {call, gsets_srv, intersection, [make_set(uod:terms(S#s.uod))]}}
           || uod:terms(S#s.uod) /= []]
      ++ [{2, {call, gsets_srv, is_subset, [make_set(uod:terms(S#s.uod))]}}
           || uod:terms(S#s.uod) /= []]
      %% union
      ++ [{2, {call, gsets_srv, union, [make_set(uod:terms(S#s.uod))]}}
          || uod:terms(S#s.uod) /= []]
      %% union with extras
      ++ [{2, {call, gsets_srv, union, [make_set(uod:terms(S#s.uod)) ++
                   [value() || _ <- lists:seq(1, rand:uniform(40))]]
              }}
          || uod:terms(S#s.uod) /= []]
      %% is_disjoint unlikely
      %% [{5, {call, gsets_srv, del_element, [take_random(uod:terms(S#s.uod))]}}
      %%  || uod:terms(S#s.uod) /= []]
      %% is_disjoint likely
      %% [{5, {call, gsets_srv, del_element, [take_random(uod:terms(S#s.uod))]}}
      %%  || uod:terms(S#s.uod) /= []]
      %% subtract
      %% [{5, {call, gsets_srv, del_element, [take_random(uod:terms(S#s.uod))]}}
      %%  || uod:terms(S#s.uod) /= []]
      ++ [{5, {call, gsets_srv, del_element, [take_random(uod:terms(S#s.uod))]}}
          || uod:terms(S#s.uod) /= []]
     ).


next_state(S, _, {call, _, add_element, [Val]}) ->
    %% UoD is deterministic so this should work.
    {_, UoD} = uod:add_term(Val, S#s.uod),
    S#s{
      set = sets:add_element(Val, S#s.set),
      uod = UoD
     };
next_state(S, _, {call, _, del_element, [Val]}) ->
    S#s{
      set = sets:del_element(Val, S#s.set)
     };
next_state(S, _, {call, _, _, _}) ->
    S.

precondition(S, {call, _, is_subset, _}) ->
    S#s.uod =/= uod:new();
precondition(_, _) ->
    true.

postcondition(S, {call, _, add_element, [Val]}, {ok, Set}) ->
    {DTerm, _} = uod:add_term(Val, S#s.uod),
    true =:= gsets:is_element(DTerm, Set);
postcondition(S, {call, _, del_element, [Val]}, {ok, Set}) ->
    case uod:get_dterm(Val, S#s.uod) of
        {ok, DTerm} ->
            false =:= gsets:is_element(DTerm, Set);
        _ ->
            %% trivially true
            true
    end;
postcondition(S, {call, _, intersection, _}, {Res, Set}) ->
    Comp = sets:intersection(S#s.set, Set),
    Res2 = lists:foldl(fun(Term, Acc) ->
                               {ok, DTerm} = uod:get_dterm(Term, S#s.uod),
                               gsets:del_element(DTerm, Acc)
                       end,
                       Res,
                       sets:to_list(Comp)),
    %% new gsets are empty gsets
    gsets:new() =:= Res2;
postcondition(S, {call, _, union, _}, {Res, Set}) ->
    Comp = sets:union(S#s.set, Set),
    Res2 = lists:foldl(fun(Term, {G, U}) ->
                               {DTerm, U1} = uod:add_term(Term, U),
                               {gsets:add_element(DTerm, G), U1}
                       end,
                       Res,
                       {sets:to_list(Comp), S#s.uod}),
    %% new gsets are empty gsets
    gsets:new() =:= Res2;
postcondition(S, {call, _, is_subset, _}, {Res, Set}) ->
    io:format("set ~p~n", [S#s.set]),
    Comp = sets:is_subset(Set, S#s.set),
    io:format("res ~p ~p~n", [Res, Comp]),
    Res =:= Comp;
postcondition(_S, _C, _R) ->
    true.
