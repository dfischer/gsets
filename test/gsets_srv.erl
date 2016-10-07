-module(gsets_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-compile(export_all).

-define(SERVER, ?MODULE).

-record(state,
        {
          uod = uod:new() :: uod:uod(),
          set = gsets:new() :: gsets:gsets()
        }).

call(M) ->
    gen_server:call(?SERVER, M, infinity).

add_element(Elt) ->
    call({add_element, Elt}).

del_element(Elt) ->
    call({del_element, Elt}).

intersection(Elts) ->
    call({intersection, Elts}).

union(Elts) ->
    call({union, Elts}).

%% not sure that this is needed
is_subset([]) ->
    {false, sets:new()};
is_subset(Elts) ->
    call({is_subset, Elts}).

get_universe() ->
    call(get_universe).

stop() ->
    call(stop).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({add_element, Elt}, _, State) ->
    {P, UoD} = uod:add_term(Elt, State#state.uod),
    S = gsets:add_element(P, State#state.set),
    io:fwrite(user, "add ~p ~p~n", [UoD, S]),
    {reply, {ok, S}, State#state{uod = UoD, set = S}};
handle_call({del_element, Elt}, _, State) ->
    case uod:get_dterm(Elt, State#state.uod) of
        {ok, DTerm} ->
            io:fwrite(user, "del found ", []),
            S = gsets:del_element(DTerm, State#state.set);
        _ ->
            io:fwrite(user, "del not found", []),
            S = State#state.set
    end,
    io:fwrite(user, "~p~n", [S]),
    {reply, {ok, S}, State#state{set = S}};
handle_call({intersection, Elts}, _, State) ->
    {GSet, Set} =
        lists:foldl(
          fun(Term, {G, S}) ->
                  {ok, DTerm} = uod:get_dterm(Term, State#state.uod),
                  G1 = gsets:add_element(DTerm, G),
                  S1 = sets:add_element(Term, S),
                  {G1, S1}
          end,
          {gsets:new(), sets:new()},
          Elts),
    Res = gsets:intersection(GSet, State#state.set),
    %% reply is the result + the set equivalent so that the model's
    %% backing set can be intersected in the postcondition
    Reply = {Res, Set},
    {reply, Reply, State};
handle_call({union, Elts}, _, State) ->
    {GSet, Set, _} =
        lists:foldl(
          fun(Term, {G, S, U}) ->
                  {DTerm, U1} = uod:add_term(Term, U),
                  G1 = gsets:add_element(DTerm, G),
                  S1 = sets:add_element(Term, S),
                  {G1, S1, U1}
          end,
          {gsets:new(), sets:new(), State#state.uod},
          Elts),
    Res = gsets:union(GSet, State#state.set),
    Reply = {Res, Set},
    {reply, Reply, State};
handle_call({is_subset, Elts}, _, State) ->
    io:fwrite(user, "subu ~p ~p ~n", [State, Elts]),
    UoD = State#state.uod,
    Reply =
        try
            {GSet, Set, _U1} =
                lists:foldl(
                  fun(Term, {G, S, U}) ->
                          {DTerm, U1} = uod:add_term(Term, U),
                          G1 = gsets:add_element(DTerm, G),
                          S1 = sets:add_element(Term, S),
                          {G1, S1, U1}
                  end,
                  {gsets:new(), sets:new(), UoD},
                  Elts),
            Res = gsets:is_subset(GSet, State#state.set),
            {Res, Set}
        catch _:_ ->
                {false, sets:add_element('unlikely sentinel', sets:new())}
        end,
    {reply, Reply, State};
handle_call(get_universe, _, State) ->
    %% this copies the whole thing, which might be slow.  do we need
    %% all of it? is there some preprocessing we can do here?
    {reply, State#state.uod, State};
handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    %% lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    %% lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    %% lager:warning("unexpected message ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
