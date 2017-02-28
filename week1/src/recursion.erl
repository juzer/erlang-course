-module(recursion).

-export([fib/1, pieces/1]).


fib(0) ->
  0;

fib(1) ->
  1;

fib(N) when N > 0 ->
  fib(N-2) + fib(N-1).


pieces(0) ->
  0;

pieces(1) ->
  2;

pieces(N) when N > 0 ->
  N + pieces(N-1).

