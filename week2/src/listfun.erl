-module(listfun).
-include_lib("eunit/include/eunit.hrl").

-export([product/1, listmax/1, productT/1]).


product([]) ->
  1;

product([X | Xs]) ->
  X * product(Xs).

product_test() ->
  1 = product([]),
  0 = product([9, 5, 4, 0]),
  1 = product([1, 1, 1]),
  -1 = product([-1]),
  -10 = product([-2, 5]),
  42 = product([2, 7, 3]).



productT([]) -> 1;

productT([X | Xs]) ->
  productT([X | Xs], 1).

productT([], P) -> P;

productT([X | Xs], P) ->
  productT(Xs, P * X).

productT_test() ->
  1 = productT([]),
  0 = productT([9, 5, 4, 0]),
  1 = productT([1, 1, 1]),
  -1 = productT([-1]),
  -10 = productT([-2, 5]),
  42 = productT([2, 7, 3]).



listmax([X]) ->
  X;

listmax([X | Xs]) ->
  max(X, listmax(Xs)).

listmax_test() ->
  0 = listmax([0]),
  0 = listmax([-8, 0]),
  -1 = listmax([-1, -2, -3]),
  3 = listmax([-1, -2, 3]),
  -1 = listmax([-1]),
  7867 = listmax([7866, 7867]).



listmaxT([X | Xs]) ->
  listmaxT([X | Xs], X).

listmaxT([], M) ->
  M;

listmaxT([X | Xs], M) ->
  listmaxT(Xs, max(X, M)).

listmaxT_test() ->
  0 = listmaxT([0]),
  0 = listmaxT([-8, 0]),
  -1 = listmaxT([-1, -2, -3]),
  3 = listmaxT([-1, -2, 3]),
  -1 = listmaxT([-1]),
  7867 = listmaxT([7866, 7867]).