-module(gset_props).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

-behaviour(proper_statem).

-export([
         command/1,
         initial_state/0,
         next_state/3,
         postcondition/3,
         precondition/2
        ]).

-export([statem/0]).

-record(s,
        {
          uod = uod:new() :: uod:uod(),
          set = sets:new() :: sets:set(),
          elements = [] :: [value()]
        }).

test() ->
    proper:quickcheck(?MODULE:statem(),
                      [{to_file, user},
                       {num_tests, 10000}]).

statem() ->
    ?FORALL(
       Cmds, commands(?MODULE),
       ?TRAPEXIT(
          begin
              gset_srv:start_link(),
              {History, State, Result} = run_commands(?MODULE, Cmds),
              gset_srv:stop(),
              ?WHENFAIL(io:format("History: ~w\n"
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
    [];
take_subset(L) ->
    Len = length(L),
    Take = random:uniform(Len),
    tks(L, Take, []).

tks(_L, 0, Acc) ->
    Acc;
tks([], _, Acc) ->
    Acc;
tks([H|T], Take, Acc) ->
    case random:uniform(2) of
        1 ->
            tks(T, Take - 1, [H|Acc]);
        _ ->
            tks(T, Take, Acc)
    end.

%% model stuff

initial_state() ->
    random:seed(now()),
    #s{}.

command(S) ->
    oneof([
           {call, gset_srv, add_element, [value()]}
          ] ++
          [
           {call, gset_srv, intersection, [make_set(S#s.elements)]}
           || S#s.elements /= []]
         ).


next_state(S, _, {call, _, add_element, [Val]}) ->
   {_, UoD} = uod:add_term(Val, S#s.uod),
    Set = sets:add_element(Val, S#s.set),
    S#s{set = Set,
        elements = [Val|S#s.elements],
        uod = UoD
       };
next_state(S, _, {call, _, intersection, _}) ->
    S.

precondition(_, _) ->
    true.

postcondition(_S, {call, _, intersection, _}, _Res) ->
    %need ot actually check something here
    true;
postcondition(_S, _C, _R) ->
    true.
