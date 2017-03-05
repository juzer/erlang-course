-module(listsexc).
-include_lib("eunit/include/eunit.hrl").

-export([double/1, evens/1, median/1, modes/1]).


double([]) -> [];

double([X | Xs]) -> [2 * X | double(Xs)].

double_test() ->
  [] = double([]),
  [0] = double([0]),
  [2] = double([1]),
  [6, 8, 90, 4] = double([3, 4, 45, 2]),
  [-6, 8] = double([-3, 4]).



evens([]) -> [];

evens([X | Xs]) ->
  case (X rem 2) of 0
    -> [X | evens(Xs)];
    _ -> evens(Xs)
  end.

evens_test() ->
  [] = evens([]),
  [] = evens([1]),
  [2] = evens([2]),
  [4, 6, 8] = evens([4, 5, 6, 7, 8]),
  [-4, 6, -8] = evens([-4, 5, 6, 7, -8]).


%% the median of a list of numbers: this is the middle element when the list is ordered
%% (if the list is of even length you should average the middle two)
median([X]) -> X;

median(X) ->
  S = lists:sort(X),
  L = length(S),
  middle(S, L).

middle(S, L) ->
  case (L rem 2) of
    0 -> sum(elems(S, (L div 2) - 1, 2)) / 2;
    1 ->
      [X | _] = elems(S, (L div 2), 1),
      X
  end.

sum([]) -> 0;

sum([X, Xs]) ->
  X + sum(Xs);

sum(X) -> X.

%% Return N elements of list S starting from index I
elems(S, 0, N) ->
  items(S, N);

elems([_ | Ss], I, N) ->
  elems(Ss, I - 1, N).

%% Return N first elements of list S
items(S, N) ->
  items(S, N, []).

items(_, 0, Acc) ->
  Acc;

items([S | Ss], N, Acc) ->
  items(Ss, N - 1, Acc ++ [S]).

median_test() ->
  2 = median([1, 2, 3]),
  2.5 = median([1, 2, 3, 4]),
  2.5 = median([2, 1, 3, 4]).


%% the modes of a list of numbers: this is a list consisting of the numbers
%% that occur most frequently in the list; if there is is just one,
%% this will be a list with one element only
modes([X]) -> [X];

modes(X) ->
  modes(X, []).

modes([], R) -> max_repetitions(R);

modes([X | Xs], R) ->
  modes(Xs, calculate_repetitions(X, R)).

calculate_repetitions(X, R) ->
  case filter(X, R, []) of
    {R, {}} -> R ++ [{X, 1}]; %% X not found
    {Rf, {X, N}} -> Rf ++ [{X, N + 1}] %% X with N repetitions found
  end.

%% If list R contains element X - returns a tuple {R-X, {X, N}}, where N is number of repetitions
%% If list R does not contain element X - returns a tuple {R, {}}
filter(_, [], RAcc) ->
  {RAcc, {}};

filter(X, [R | Rs], RAcc) ->
  case R of
    {X, N} -> {RAcc ++ Rs, {X, N}};
    _ -> filter(X, Rs, RAcc ++ [R])
  end.

max_repetitions([{X, N} | Rs]) ->
  max_repetitions(Rs, N, [X]).

max_repetitions([], _, Max) ->
  Max;

max_repetitions([{X, N} | Rs], NMax, _) when (N > NMax) ->
  max_repetitions(Rs, N, [X]);

max_repetitions([{X, N} | Rs], NMax, Max) when (N == NMax) ->
  max_repetitions(Rs, N, Max ++ [X]);

max_repetitions([{_, N} | Rs], NMax, Max) when (N < NMax) ->
  max_repetitions(Rs, NMax, Max).


modes_test() ->
  [1] = modes([1, 1, 2]),
  [2] = modes([2, 1, 2, 1, 1, 2, 2]),
  [3, 4] = modes([3, 3, 4, 4, 1]).