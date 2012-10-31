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

%% @doc

-module(fn).

-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([arity/1,
         identity/1,
         constantly/1,
         constantly/2,
         complement/1,
         comp/1,
         comp/2,
         partial/2,
         flip/1]).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Return the arity of the function argument
-spec arity(fun((...) -> term())) -> non_neg_integer().
arity(Fun) ->
    element(2, erlang:fun_info(Fun, arity)).

%% @doc Return the input
-spec identity(A) -> A.
identity(Input) ->
    Input.

%% @doc Return a zero arity function that returns
%% `Val'.
-spec constantly(A) -> fun(() -> A).
constantly(Val) ->
    constantly(0, Val).

%% @doc Return a function that ignores it's arguments
%% and always returns `Val'. The returned function
%% has arity `Arity'.
-spec constantly(non_neg_integer(), A) ->
    fun((...) -> A).
constantly(Arity, Val) ->
    call_with_arglist(Arity, constantly_helper(Val)).

%% @private
constantly_helper(Val) ->
    fun(_Args) ->
            Val
    end.

%% @doc Return a function that calls `Fun' but returns
%% the opposite boolean value. Works with functions of
%% any arity.
-spec complement(fun((...) -> boolean())) -> fun((...) -> boolean).
complement(Fun) ->
    FunArity = arity(Fun),
    call_with_arglist(FunArity, complement_helper(Fun)).

%% @private
-spec complement_helper(fun((...) -> boolean())) -> fun((list()) -> boolean()).
complement_helper(Fun) ->
    fun (Args) ->
            not erlang:apply(Fun, Args)
    end.

%% @doc Compose a list of functions together, right-to-left.
%% Returns a function with arity equal to the last function.
-spec comp([fun()]) -> fun((...) -> term()).
comp([Fun]) ->
    constantly(Fun);
comp(Funs) ->
    lists:foldl(fn:flip(fun comp/2),
                hd(Funs),
                tl(Funs)).

%% @doc Compose two functions `FunA' and `FunB' together.
%% Returns a function that has the same arity as `FunB'.
%% The resulting function calls `FunB' and then `FunA'
%% with the result of calling `FunA'.
-spec comp(fun((...) -> term()), fun((...) -> B)) ->
               fun((...) -> B).
comp(FunA, FunB) ->
    Arity = arity(FunB),
    call_with_arglist(Arity, comp_helper(FunA, FunB)).

%% @private
-spec comp_helper(fun((...) -> term()), fun((...) -> B))
                      -> fun((list()) -> B).
comp_helper(FunA, FunB) ->
    fun (Args) ->
            FunA(erlang:apply(FunB, Args))
    end.

%% @doc Return a function with one-less arity than `Fun'
%% with `Arg' implicitly as the first argument.
-spec partial(fun((...) -> A), term()) -> fun((...) -> A).
partial(Fun, Arg) ->
    FunArity = arity(Fun),
    call_with_arglist(FunArity - 1, partial_helper(Fun, Arg)).

%% private
-spec partial_helper(fun((...) -> A), term()) -> fun((list()) -> A).
partial_helper(Fun, Arg) ->
    fun (RestArgsAsList) ->
            erlang:apply(Fun, [Arg | RestArgsAsList])
    end.

%% @doc Flip the arguments of a binary function.
-spec flip(fun((A, B) -> C)) -> fun((B, A) -> C).
flip(Fun) ->
    fun (A, B) ->
            Fun(B, A)
    end.

%%%===================================================================
%%% Private shared functions
%%%===================================================================

call_with_arglist(0, Fun) ->
    fun () ->
            Fun([])
    end;
call_with_arglist(1, Fun) ->
    fun (A) ->
            Fun([A])
    end;
call_with_arglist(2, Fun) ->
    fun (A, B) ->
            Fun([A, B])
    end;
call_with_arglist(3, Fun) ->
    fun (A, B, C) ->
            Fun([A, B, C])
    end;
call_with_arglist(4, Fun) ->
    fun (A, B, C, D) ->
            Fun([A, B, C, D])
    end;
call_with_arglist(5, Fun) ->
    fun (A, B, C, D, E) ->
            Fun([A, B, C, D, E])
    end.
