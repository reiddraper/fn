%% @doc An implementation of the Either monad, but with an emphasis on being
%% compatible with existing Erlang code. Hence using `{ok, ...}' and
%% `{error, ...}' instead of `Right' and `Left' (`Right' is success, `Left'
%% is failure, traditionally). Otherwise, nomenclature
%% should be identical to Haskell.

-module(either).


-export_type([either/2]).
-export([return/1,
         bind/2,
         lift/2,
         kleisli/1,
         kleisli/2,
         pipe/2,
         sequence/1]).

-type either(A, B) :: {ok, A} | {error, B}.

-spec return(A) -> either(A, term()).
%% @doc Wrap `A' in `{ok, A}'. Put another way, returns puts `A' into the
%% Either monad.
return(A) ->
    {ok, A}.

-spec bind(either(A, B), fun((A) -> either(A, B))) -> either(A, B).
%% @doc Apply `F' to the value inside the monad if it's `{ok, ...}',
%% otherwise just return the error tuple.
bind({error, _Error}=E, _F) ->
    E;
bind({ok, Value}, F) ->
    F(Value).

-spec lift(either(A, Z), fun((A) -> C)) -> either(C, Z).
%% @doc Lets you use a function to act on an Either monad, but your function
%% can return a value. It will automatically be wrapped back into an
%% `{ok, ...}' tuple.
lift(M, F) ->
    bind(M, fun (A) -> return(F(A)) end).

-spec kleisli([fun()]) -> fun((A) -> either(A, _B)).
%% @doc Like `kleisli/2' but takes a list of functions.
kleisli(Functions) ->
    lists:foldl(fun kleisli/2, fun return/1, Functions).

-spec kleisli(fun((A) -> either(B, Z)), fun((B) -> either(C, Z))) ->
                  fun((A) -> either(C, Z)).
%% @doc Kleisli Monad composition. Left to right.
kleisli(F, G) ->
    fun (X) ->
            bind(F(X), G)
    end.

-spec pipe(either(A, Z), [fun((A) -> C)]) -> either(C, Z).
%% @doc Lets you chain monadic function to apply to `M'. You might use it
%% like `pipe({ok, 5}, [Increment, Increment, Increment])'.
pipe(M, Funs) ->
    lists:foldl(fn:flip(fun bind/2), M, Funs).

-spec sequence([either(A, B)]) -> either([A], B).
%% @doc Turn a list of `either(A, B)' into `either([A], B)'. Put another way,
%% turns `[{ok, 5}, {ok, 6}]' into `{ok, [5, 6]}'. If there are _any_ errors,
%% then that is returned (returns the first error in the list).
%% So `[{ok, 5}, {error, foo}, {error, bar}, {ok, 7}]' returns
%% `{error, foo}'.
sequence(Eithers) ->
    %% `foldlr' to avoid reversing
    lists:foldr(fun sequence_fold/2, return([]), Eithers).

sequence_fold(Elem, Acc) ->
    bind(Elem, fun(Item) ->
                bind(Acc, fun (AccList) ->
                            return([Item | AccList]) end) end).
