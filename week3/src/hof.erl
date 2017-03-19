-module(hof).

-include_lib("eunit/include/eunit.hrl").
-export([]).

doubleAll([]) -> [];
doubleAll([X | Xs]) ->
  [2 * X | doubleAll(Xs)].

doubleAll_test() ->
  [2, 4, 6, 8] = doubleAll([1, 2, 3, 4]),
  [0, 0, 0] = doubleAll([0, 0, 0]),
  [154] = doubleAll([77]),
  [] = doubleAll([]),
  [-2, -4, -6] = doubleAll([-1, -2, -3]).



doubleAll_hof(L) -> lists:map(fun(X) -> 2 * X end, L).

doubleAll_hof_test() ->
  [2, 4, 6, 8] = doubleAll_hof([1, 2, 3, 4]),
  [0, 0, 0] = doubleAll_hof([0, 0, 0]),
  [154] = doubleAll_hof([77]),
  [] = doubleAll_hof([]),
  [-2, -4, -6] = doubleAll_hof([-1, -2, -3]).



evens([]) -> [];
evens([X | Xs]) when X rem 2 == 0 ->
  [X | evens(Xs)];
evens([_ | Xs]) ->
  evens(Xs).

evens_test() ->
  [2, 4, 6] = evens([1, 2, 3, 4, 5, 6, 7]),
  [-2, -4] = evens([-1, -2, -3, -4, -5]),
  [] = evens([]),
  [] = evens([9]),
  [] = evens([1, 3, 5, 7]),
  [2] = evens([1, 3, 5, 2, 7]).



evens_hof(L) -> lists:filter(fun(X) -> X rem 2 == 0 end, L).

evens_hof_test() ->
  [2, 4, 6] = evens_hof([1, 2, 3, 4, 5, 6, 7]),
  [-2, -4] = evens_hof([-1, -2, -3, -4, -5]),
  [] = evens_hof([]),
  [] = evens_hof([9]),
  [] = evens_hof([1, 3, 5, 7]),
  [2] = evens_hof([1, 3, 5, 2, 7]).



product([]) -> 1;
product([X | Xs]) -> X * product(Xs).

product_test() ->
  1 = product([]),
  1 = product([1, 1, 1]),
  2 = product([2, 1]),
  -12 = product([-3, 4]),
  12 = product([-3, -4]),
  12 = product([3, 4]),
  0 = product([3, 4, 6, -9, 0]).



product_hof(L) -> lists:foldr(fun(X, Y) -> X * Y end, 1, L).

product_hof_test() ->
  1 = product_hof([]),
  1 = product_hof([1, 1, 1]),
  2 = product_hof([2, 1]),
  -12 = product_hof([-3, 4]),
  12 = product_hof([-3, -4]),
  12 = product_hof([3, 4]),
  0 = product_hof([3, 4, 6, -9, 0]).




zip(X, Y) ->
  zip(X, Y, []).

zip([X | Xs], [Y | Ys], R) ->
  zip(Xs, Ys, [{X, Y} | R]);

zip(_, _, R) -> lists:reverse(R).

zip_test() ->
  [{1, 2}, {3, 4}] = zip([1, 3, 5, 7], [2, 4]).



zip_with(F, X, Y) ->
  zip_with(F, X, Y, []).

zip_with(F, [X | Xs], [Y | Ys], R) ->
  zip_with(F, Xs, Ys, [F(X, Y) | R]);

zip_with(_, _, _, R) -> lists:reverse(R).

zip_with_test() ->
  [3, 7] = zip_with(fun(X, Y) -> X + Y end, [1, 3, 5, 7], [2, 4]).



zip_with_map(F, X, Y) -> lists:map(fun({A, B}) -> F(A, B) end, zip(X, Y)).

zip_with_map_test() ->
  [3, 7] = zip_with_map(fun(X, Y) -> X + Y end, [1, 3, 5, 7], [2, 4]).



zip_zip(X, Y) -> zip_with(fun(A, B) -> {A, B} end, X, Y).

zip_zip_test() ->
  [{1, 2}, {3, 4}] = zip_zip([1, 3, 5, 7], [2, 4]).