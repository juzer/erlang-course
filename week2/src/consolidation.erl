-module(consolidation).
-export([]).
-include_lib("eunit/include/eunit.hrl").



join(X, Y) ->
  merge(lists:reverse(X), Y).

merge([], Y) -> Y;

merge([X | Xs], Y) ->
  merge(Xs, [X | Y]).

join_test() ->
  "hello" = join("hel", "lo"),
  "hello" = join("hello", []),
  "hello" = join([], "hello"),
  "abbbba" = join("abb", "bba").



concat([]) -> [];

concat([X | Xs]) ->
  join(X, concat(Xs)).

concat_test() ->
  "goodbye" = concat(["goo", "d", "by", "e"]).



member(_, []) -> false;

member(X, [X | _]) -> true;

member(X, [_Y | Ys]) -> member(X, Ys).

member_test() ->
  true = member(2, [2, 0, 0, 1]),
  false = member(20, [2, 0, 0, 1]),
  false = member(20, []).



merge_sort([]) -> [];
merge_sort([X]) -> [X];
merge_sort([X, Y]) when X =< Y -> [X, Y];
merge_sort([X, Y]) when X > Y -> [Y, X];

merge_sort(X) ->
  {L, R} = lists:split(length(X) div 2, X),
  merge_sort_halves(merge_sort(L), merge_sort(R)).

merge_sort_halves([], Y) -> Y;
merge_sort_halves(X, []) -> X;
merge_sort_halves([X | Xs], [Y | Ys]) when X =< Y -> [X | merge_sort_halves(Xs, [Y | Ys])];
merge_sort_halves([X | Xs], [Y | Ys]) when X > Y -> [Y | merge_sort_halves([X | Xs], Ys)].

merge_sort_test() ->
  [1, 2, 3, 4, 5] = merge_sort([2, 5, 1, 3, 4]),
  [1, 2, 3, 4, 5, 6] = merge_sort([4, 5, 1, 3, 2, 6]),
  [1, 2, 3, 4, 5, 6] = merge_sort([1, 2, 3, 4, 5, 6]),
  [1, 1, 1, 1, 6] = merge_sort([1, 6, 1, 1, 1]).



quick_sort([]) -> [];

quick_sort([X]) -> [X];

quick_sort([X | Xs]) ->
  subsort(X, Xs, [], []).

subsort(P, [], L, R) -> join(quick_sort(L), [P | quick_sort(R)]);

subsort(P, [X | Xs], L, R) when X < P ->
  subsort(P, Xs, [X | L], R);

subsort(P, [X | Xs], L, R) ->
  subsort(P, Xs, L, [X | R]).

quick_sort_test() ->
  [1, 2, 3, 4, 5] = quick_sort([2, 5, 1, 3, 4]),
  [1, 2, 3, 4, 5, 6] = quick_sort([4, 5, 1, 3, 2, 6]),
  [1, 2, 3, 4, 5, 6] = quick_sort([1, 2, 3, 4, 5, 6]),
  [1, 1, 1, 1, 6] = quick_sort([1, 6, 1, 1, 1]).



selection_sort([]) -> [];

selection_sort(X) ->
  select(X, []).

select([], S) -> S;

select([X | Xs], S) ->
  select(Xs, insert(X, S)).

insert(X, []) -> [X];

insert(X, [S | Ss]) when X =< S ->
  [X | [S | Ss]];

insert(X, [S | Ss]) ->
  [S | insert(X, Ss)].


selection_sort_test() ->
  [1, 2, 3, 4, 5] = selection_sort([2, 5, 1, 3, 4]),
  [1, 2, 3, 4, 5, 6] = selection_sort([4, 5, 1, 3, 2, 6]),
  [1, 2, 3, 4, 5, 6] = selection_sort([1, 2, 3, 4, 5, 6]),
  [1, 1, 1, 1, 6] = selection_sort([1, 6, 1, 1, 1]).
