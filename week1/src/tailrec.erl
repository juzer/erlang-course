-module(tailrec).

-export([fib/1, perf/1]).


fib(N) when N >= 0 -> fib(N,0,1).

fib(0,A,_) ->
  A;

fib(N,A,B) when N>0 ->
  fib(N-1,B,A+B).



perf(N) when N > 0 ->
  perf(N, N-1, 0).

perf(N, 0, N) ->
  true;

perf(_N, 0, _A) ->
  false;

perf(N, C, A) when N rem C == 0 ->
  perf(N, C-1, A+C);

perf(N, C, A) ->
  perf(N, C-1, A).
