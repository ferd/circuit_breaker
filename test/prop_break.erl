-module(prop_break).
-include_lib("proper/include/proper.hrl").
-compile(export_all).
-define(DEFAULT_LIMIT, 3).

prop_test() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
        ?TRAPEXIT(
            begin
                {ok, Pid} = circuit_breaker:start_link(),
                {History, State, Result} = proper_fsm:run_commands(?MODULE, Cmds),
                gen_server:stop(Pid),
                ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                    [History,State,Result]),
                          aggregate(zip(proper_fsm:state_names(History),
                                        command_names(Cmds)),
                                    Result =:= ok))
            end)
    ).

-record(data, {
          limit :: pos_integer(),
          registered = false :: boolean(),
          errors = 0 :: pos_integer(),
          timeouts = 0 :: pos_integer()
        }).

initial_state() ->
    ok.

initial_state_data() ->
    #data{limit = ?DEFAULT_LIMIT}.

ok(_Data) ->
    [{history, {call, shim_break, success, []}},
     {history, {call, shim_break, err, [valid_error()]}},
     {tripped, {call, shim_break, err, [valid_error()]}},
     {history, {call, shim_break, ignored_error, [ignored_error()]}},
     {history, {call, shim_break, timeout, []}},
     {tripped, {call, shim_break, timeout, []}},
     {blocked, {call, shim_break, manual_block, []}},
     {ok, {call, shim_break, manual_deblock, []}},
     {ok, {call, shim_break, manual_reset, []}}].

tripped(_Data) ->
    [{history, {call, shim_break, success, []}},
     {history, {call, shim_break, err, [valid_error()]}},
     {history, {call, shim_break, ignored_error, [ignored_error()]}},
     {history, {call, shim_break, timeout, []}},
     {ok, {call, shim_break, manual_deblock, []}},
     {ok, {call, shim_break, manual_reset, []}},
     {blocked, {call, shim_break, manual_block, []}}].

blocked(_Data) ->
    [{history, {call, shim_break, success, []}},
     {history, {call, shim_break, err, [valid_error()]}},
     {history, {call, shim_break, ignored_error, [ignored_error()]}},
     {history, {call, shim_break, timeout, []}},
     {history, {call, shim_break, manual_block, []}},
     {history, {call, shim_break, manual_reset, []}},
     {ok, {call, shim_break, manual_deblock, []}}].

%% v ADDED
weight(ok, tripped, _) -> 5;
weight(ok, ok, {call, _, F, _}) ->
    case F of
        error -> 4;
        timeout -> 4;
        _ -> 1
    end;
weight(_, _, _) -> 1.
%% ^ ADDED

%% Picks whether a command should be valid under the current state.
precondition(_From, _To, Data, {call, _, manual_reset, _}) ->
    Data#data.registered;
precondition(_From, _To, Data, {call, _, manual_block, _}) ->
    Data#data.registered;
precondition(_From, _To, Data, {call, _, manual_deblock, _}) ->
    Data#data.registered;
precondition(ok, To, #data{errors=N, limit=L}, {call,_,err,_}) ->
    (To =:= tripped andalso N+1 =:= L) orelse (To =:= ok andalso N+1 =/= L);
precondition(ok, To, #data{timeouts=N, limit=L}, {call,_,timeout,_}) ->
    (To =:= tripped andalso N+1 =:= L) orelse (To =:= ok andalso N+1 =/= L);
precondition(ok, ok, _Data, _Call) ->
    true;
precondition(tripped, _, _Data, _Call) ->
    true;
precondition(blocked, _, _Data, _Call) ->
    true.

%% Given the state `State' *prior* to the call `{call, Mod, Fun, Args}',
%% determine whether the result `Res' (coming from the actual system)
%% makes sense.
postcondition(tripped, tripped, _Data, _Call, {error, {circuit_breaker, _}}) ->
    true;
postcondition(_, blocked, _Data, {call, _, manual_block, _}, ok) ->
    true;
postcondition(_, blocked, _Data, _Call, {error, {circuit_breaker, _}}) ->
    true;
postcondition(_, ok, _Data, {call, _, success, _}, success) ->
    true;
postcondition(_, ok, _Data, {call, _, manual_deblock, _}, ok) ->
    true;
postcondition(_, _, _Data, {call, _, manual_reset, _}, ok) ->
    true;
postcondition(ok, _, _Data, {call, _, timeout, _}, {error, timeout}) ->
    true;
postcondition(ok, _, _Data, {call, _, err, _}, {error, Err}) ->
    not lists:member(Err, [ignore1, ignore2]);
postcondition(ok, _, _Data, {call, _, ignored_error, _}, {error,Err}) ->
    lists:member(Err, [ignore1, ignore2]);
postcondition(From, To, _Data, {call, _Mod, Fun, Args}, Res) ->
    io:format("unexpected postcondition: ~p -> ~p ({~p,~p}) = ~p~n",
              [From, To, Fun, Args, Res]),
    false.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state_data(ok, _To, Data=#data{errors=N}, _Res, {call,_,err,_}) ->
    Data#data{errors=N+1, registered=true};
next_state_data(ok, _To, Data=#data{timeouts=N}, _Res, {call,_,timeout,_}) ->
    Data#data{timeouts=N+1, registered=true};
%% v ADDED
next_state_data(ok, _To, Data=#data{errors=N, timeouts=M}, _Res,
                {call,_,F,_}) when F == success; F == ignored_error ->
    if N > 0 -> Data#data{errors=N-1};
       M > 0 -> Data#data{timeouts=M-1};
       N =:= 0, M =:= 0 -> Data#data{registered=true}
    end;
%% ^ ADDED
next_state_data(_From, _To, Data, _Res, {call,_,manual_deblock,_}) ->
    Data#data{errors=0, timeouts=0};
next_state_data(_From, _To, Data, _Res, {call,_,manual_reset,_}) ->
    Data#data{errors=0, timeouts=0};
next_state_data(_From, _To, Data, _res, {call,_,manual_block,_}) ->
    Data;
next_state_data(_From, _To, Data, _Res, {call, _Mod, _Fun, _Args}) ->
    Data#data{registered=true}. % all calls but manual ones register

%% Generators
valid_error() -> elements([badarg, badmatch, badarith, whatever]).
ignored_error() -> elements([ignore1, ignore2]).

