-module(second).
-export([hypotenuse/2, perimter/2, area/2]).

hypotenuse(A, B) ->
  math:sqrt(first:square(A) + first:square(B)).

perimter(A, B) ->
  A+B+hypotenuse(A,B).

area(A,B) ->
  % A*B/2
  % fist:mult(first:mult(A,B), 0.5)
  first:area(A,B,hypotenuse(A,B)).