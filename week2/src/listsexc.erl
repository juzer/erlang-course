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
median(X) ->
  S = lists:sort(X),
  middle(S, length(S)).

%% Finds the middle element(s) of list S
middle(S, L) when (L rem 2) == 0 ->
  sum(elems(S, (L div 2) - 1, 2)) / 2;

middle(S, L) when (L rem 2) == 1 ->
  [X | _] = elems(S, (L div 2), 1),
  X.

sum([]) -> 0;

sum([X | Xs]) ->
  X + sum(Xs).

%% Return N elements of list S starting from index I
elems([S1 | _], 0, 1) -> [S1];

elems([S1, S2 | _], 0, 2) -> [S1, S2];

elems([_ | Ss], I, N) ->
  elems(Ss, I - 1, N).

median_test() ->
  2 = median([1, 2, 3]),
  2.5 = median([1, 2, 3, 4]),
  2.5 = median([2, 1, 3, 4]).


%% the modes of a list of numbers: this is a list consisting of the numbers
%% that occur most frequently in the list; if there is is just one,
%% this will be a list with one element only
modes(X) ->
  modes(X, []).

modes([], R) -> max_repetitions(R);

modes([X | Xs], R) ->
  modes(Xs, calculate_repetitions(X, R)).

%% Create a list of tuples {X, N}, where X is a unique item from list R and N is the number of its repetitions in R
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

%% Get a list of items with highest repetitions
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