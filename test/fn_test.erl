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

%% Constantly

constantly_suite_test_() ->
    {setup,
     fun () -> ok end,
     [constantly_builder(Arity, Val) || Arity <- lists:seq(0, 3),
                                        Val <- list_of_terms()]
    }.

constantly_builder(Arity, Val) ->
    Constantly = fn:constantly(Arity, Val),
    fun () ->
            Result = eval_arity(Arity, Constantly),
            ?assertEqual(Result, Val)
    end.

eval_arity(0, Fun) ->
    Fun();
eval_arity(1, Fun) ->
    Fun(foo);
eval_arity(2, Fun) ->
    Fun(foo, bar);
eval_arity(3, Fun) ->
    Fun(foo, bar, baz).


%% Complement

truthy() ->
    true.

falsy() ->
    false.

truthy_test() ->
    ?assertEqual(truthy(),
                 not (fn:complement(fun truthy/0))()).

falsy_test() ->
    ?assertEqual(truthy(),
                 not (fn:complement(fun truthy/0))()).

%% Partial

partial_1_test() ->
    Fun = fun erlang:'+'/2,
    ?assertEqual((fn:partial(Fun, 5))(7), 12).

partial_2_test() ->
    Fun = fun erlang:'+'/2,
    ?assertEqual((fn:partial(Fun, 1))(1), 2).

partial_3_test() ->
    Fun = fun (A, B, C) -> lists:foldl(fun erlang:'+'/2, 0, [A, B, C]) end,
    ?assertEqual((fn:partial(Fun, 3))(5, 7), 15).

partial_4_test() ->
    Fun = fun (A, B, C) -> lists:foldl(fun erlang:'+'/2, 0, [A, B, C]) end,
    PartialOne = fn:partial(Fun, 3),
    ?assertEqual((fn:partial(PartialOne, 5))(7), 15).

%% Flip

integer_pairs() ->
    [{1, 2},
     {5, 99},
     {10, 5},
     {10, 4},
     {77, 77},
     {90, 1001}].

flip_builder(A, B) ->
    %% `Fun' must not be commutative
    Fun = fun erlang:'/'/2,
    fun () ->
            ?assertEqual(erlang:apply(Fun, [A, B]),
                         erlang:apply(fn:flip(Fun), [B, A]))
    end.

flip_suite_test_() ->
    {setup,
     fun () -> ok end,
     [flip_builder(A, B) || {A, B} <- integer_pairs()]
    }.

-endif.
