-module(hof2).
-export([add/1, times/1, compose/2, id/1, iterate/1]).

-include_lib("eunit/include/eunit.hrl").


compose_list(X) ->
  fun(Z) -> lists:foldl(fun(A, B) -> A(B) end, Z, X) end.

compose_list_test() ->
  9 = (compose_list([fun(X) -> 2 * X end, fun(X) -> X + 5 end]))(2).



twice(F) ->
  compose(F, F).

twice_test() ->
  18 = (twice(fun(X) -> X * 3 end))(2),
  162 = (twice(twice(fun(X) -> X * 3 end)))(2).



add(X) ->
  fun(Y) -> X + Y end.

times(X) ->
  fun(Y) ->
    X * Y end.

compose(F, G) ->
  fun(X) -> G(F(X)) end.

id(X) ->
  X.

iterate(0) ->
  fun(_) -> (fun id/1) end;

iterate(N) ->
  fun(F) -> compose_list(lists:duplicate(N, F)) end.

iterate_test() ->
  42 = ((iterate(0))(add(100)))(42),
  32 = ((iterate(1))(add(-10)))(42),
  50 = ((iterate(8))(add(1)))(42),
  24 = ((iterate(3))(times(2)))(3).

	     

