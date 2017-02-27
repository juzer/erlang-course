-module(patterns).

-export([xOr1/2, xOr2/2, xOr3/2, maxThree/3, howManyEq/3]).

xOr1(X,Y) ->
  not X == Y.

xOr2(X,Y) ->
  X =/= Y.

xOr3(X,Y) ->
  (X or Y) and not (X and Y).

maxThree(X,Y,Z) ->
  max(X, max(Y,Z)).


howManyEq(X,X,X) ->
  3;

howManyEq(X,X,_) ->
  2;

howManyEq(X,_,X) ->
  2;

howManyEq(_,X,X) ->
  2;

howManyEq(_,_,_) ->
  0.
