-module(either_eqc).

-compile(export_all).

-ifdef(EQC).

-define(NUM_TESTS, 1000).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Monad law tests
%% ---------------------------------------------------------------------------

prop_left_identity() ->
    numtests(?NUM_TESTS,
             ?FORALL({I, F}, {int(), either_fun()},
                     either:bind(either:return(I), F) ==
                     F(I))).

prop_right_identity() ->
    numtests(?NUM_TESTS,
             ?FORALL(I, int(),
                     either:bind({ok, I}, fun either:return/1) ==
                     {ok, I})).

prop_associative() ->
    numtests(?NUM_TESTS,
             ?FORALL({I, F, G}, {int(), either_fun(), either_fun()},
                     begin
                M = {ok, I},
                either:bind(either:bind(M, F), G) ==
                either:bind(M, fun (A) -> either:bind(F(A), G) end)
            end)).

%% Kleisli Composition Equivalent
%% ---------------------------------------------------------------------------

%% Note this is more or less the same as the `prop_associative' test.
prop_kleisli() ->
    numtests(?NUM_TESTS,
             ?FORALL({I, F, G}, {int(), either_fun(), either_fun()},
                     begin
                M = either:return(I),
                either:bind(either:bind(M, F), G) ==
                either:bind(M, either:kleisli(F, G))
            end)).


%% Generators
%% ---------------------------------------------------------------------------

either_fun() ->
    ?LET(X, function1(int()), fun (A) -> either:return(X(A)) end).

-endif.
