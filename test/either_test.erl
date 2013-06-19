%% -------------------------------------------------------------------
%% Copyright (c) 2012 Reid Draper. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(either_test).

-compile(export_all).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% Test helpers
%% ---------------------------------------------------------------------------

increment(A) ->
    A + 1.

incrementM(A) ->
    either:return(increment(A)).

%% Tests
%% ---------------------------------------------------------------------------

lift_1_test() ->
    ?assertEqual({ok, 6},
                 either:lift({ok, 5}, fun increment/1)).

lift_2_test() ->
    Error = {error, "oh noes"},
    ?assertEqual(Error,
                 either:lift(Error, fun increment/1)).

%% pipe test
input_and_answers() ->
    Error = {error, "error"},
    IncM = fun incrementM/1,
    Fail = fun (_A) -> Error end,
    [{Error, [IncM, IncM, IncM], Error},
     {{ok, 5}, [IncM, IncM, IncM], {ok, 8}},
     {{ok, 5}, [IncM, IncM, IncM, Fail], Error}].

pipe_builder({Input, Pipe, ExpectedAnswer}) ->
    fun () ->
            ?assertEqual(ExpectedAnswer,
                 either:pipe(Input, Pipe))
    end.

pipe_test_() ->
    {setup,
     fun () -> ok end,
     [pipe_builder(A) || A <- input_and_answers()]
    }.

-endif.
