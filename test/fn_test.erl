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

-module(fn_test).

-compile(export_all).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

list_of_terms() ->
    [1,
     2,
     "",
     "a",
     "b",
     <<"abc">>,
     <<"">>,
     self(),
     node()].

%% Arity

arity1_test() ->
    F1 = fun (_A) -> ok end,
    ?assertEqual(fn:arity(F1), 1).

arity2_test() ->
    F2 = fun (_A, _B) -> ok end,
    ?assertEqual(fn:arity(F2), 2).

arity3_test() ->
    F3 = fun (_A, _B, _C) -> ok end,
    ?assertEqual(fn:arity(F3), 3).

arity4_test() ->
    F4 = fun (_A, _B, _C, _D) -> ok end,
    ?assertEqual(fn:arity(F4), 4).

%% Identity

identity_suite_test_() ->
    {setup,
     fun () -> ok end,
     [identity_builder(V) || V <- list_of_terms()]
    }.

identity_builder(Value) ->
    fun () ->
            ?assertEqual(Value, fn:identity(Value))
    end.

-endif.
