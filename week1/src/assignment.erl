-module(assignment).
-include_lib("eunit/include/eunit.hrl").

-export([area/1, bits/1, perimeter/1, bitsT/1]).


%% Perimeter function
perimeter({circle, {X, Y}, R}) when R >= 0 ->
  2 * math:pi() * R;

perimeter({rectangle, {X, Y}, W, H}) when ((W >= 0) and (H >= 0)) ->
  2 * W + 2 * H;

%% triangle represented as coordinates of 3 veritces
perimeter({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
  A = math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)),
  B = math:sqrt(math:pow(X3 - X2, 2) + math:pow(Y3 - Y2, 2)),
  C = math:sqrt(math:pow(X1 - X3, 2) + math:pow(Y1 - Y3, 2)),
  A + B + C.

%% helper function for rounding when evaluating results
round(Number, Precision) ->
  P = math:pow(10, Precision),
  round(Number * P) / P.

%% Perimeter function tests
perimeter_circle_test() ->
  25.13 = round(perimeter({circle, {0, 0}, 4}), 2).

perimeter_rectangle_test() ->
  22 = perimeter({rectangle, {0, 0}, 4, 7}).

perimeter_triangle_test() ->
  19.04 = round(perimeter({triangle, {-3, 8}, {5, 6}, {-1, 4}}), 2),
  20.24 = round(perimeter({triangle, {0, 5}, {5, 4}, {-4, 1}}), 2).




%% Area function
area({circle, {X, Y}, R}) ->
  math:pi() * R * R;

area({rectangle, {X, Y}, W, H}) ->
  W * H;

area({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
  abs(X1 * (Y2 - Y3) + X2 * (Y3 - Y1) + X3 * (Y1 - Y2)) / 2.

%% Area function tests
area_circle_test() ->
  153.94 = round(area({circle, {0, 0}, 7}), 2).

area_rectangle_test() ->
  28 = area({rectangle, {0, 0}, 4, 7}).

area_triangle_test() ->
  80.0 = area({triangle, {-5, 7}, {6, 2}, {-4, -8}}),
  19.5 = area({triangle, {1, 5}, {7, 8}, {4, 0}}).





%% Enclosing rectangle function
enclose({circle, {X, Y}, R}) ->
  {rectangle, {X - R, Y - R}, 2 * R, 2 * R};

enclose({rectangle, {X, Y}, W, H}) ->
  {rectangle, {X, Y}, W, H};

enclose({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
  %% bottom left vertex
  X = min(min(X1, X2), X3),
  Y = min(min(Y1, Y2), Y3),
  %% top right vertex
  Xt = max(max(X1, X2), X3),
  Yt = max(max(Y1, Y2), Y3),
  {rectangle, {X, Y}, Xt - X, Yt - Y}.

%% Enclosing rectangle function tests
enclose_circle_test() ->
  {rectangle, {-8, -5}, 18, 18} = enclose({circle, {1, 4}, 9}).

enclose_rectangle_test() ->
  {rectangle, {7, -1}, 3, 15} = enclose({rectangle, {7, -1}, 3, 15}).

enclose_triangle_test() ->
  {rectangle, {-3, 4}, 8, 4} = enclose({triangle, {-3, 8}, {5, 6}, {-1, 4}}).





%% Bit sum function - recursive version
bits(0) -> 0;

bits(N) when N > 0 ->
  %% get least significant bit and shift right
  (N band 1) + bits(N bsr 1).

bits_test() ->
  1 = bits(1),
  1 = bits(2),
  2 = bits(5),
  3 = bits(7),
  1 = bits(8).





%% Bit sum function - tail recursive version
bitsT(N) when N > 0 ->
  bitsT(N, N, 0).

bitsT(0, _, A) ->
  A;

%% N - counter, M - current value, A - accumulated sum
bitsT(N, M, A) ->
  bitsT(N - 1, M bsr 1, A + (M band 1)).

bitsT_test() ->
  1 = bitsT(1),
  1 = bitsT(2),
  2 = bitsT(5),
  3 = bitsT(7),
  1 = bitsT(8).