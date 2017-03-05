-module(functions).
-export([take/2, nub/1, palindrome/1]).
-include_lib("eunit/include/eunit.hrl").

-spec take(integer(), [T]) -> [T].

take(0, _) -> [];

take(_, []) -> [];

take(N, [L | Ls]) when N > 0 ->
  [L | take(N - 1, Ls)].


take_test() ->
  [] = take(0, "hell"),
  "hell" = take(4, "hello"),
  "hello" = take(5, "hello"),
  "hello" = take(9, "hello").



nub([]) -> [];

nub(X) ->
  nub(X, []).

nub([], Acc) -> lists:reverse(Acc);

nub([X | Xs], Acc) ->
  case contains(X, Acc) of
    true -> nub(Xs, Acc);
    false -> nub(Xs, [X | Acc])
  end.

contains(_, []) -> false;

contains(X, [X | _]) -> true;

contains(X, [_ | Ls]) -> contains(X, Ls).

nub_test() ->
  [2, 4, 1, 3] = nub([2, 4, 1, 3, 3, 1]).



palindrome(S) ->
  N = normalize(S),
  equal(N, lists:reverse(N)).

normalize([]) -> [];

normalize([S | Ss]) when ((S >= $a) andalso (S =< $z)) ->
  [S | normalize(Ss)];

normalize([S | Ss]) when ((S >= $A) andalso (S =< $Z)) ->
  [S + 32 | normalize(Ss)];

normalize([_ | Ss]) ->
  normalize(Ss).

equal(X, X) -> true;

equal(_X, _Y) -> false.


palindrome_test() ->
  true = palindrome("Madam I\'m Adam"),
  false = palindrome("bAAbo   "),
  true = palindrome(" .ijtu=++ +; uT,Ji75").
