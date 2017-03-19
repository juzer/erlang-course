-module(extra).
-export([]).
-include_lib("eunit/include/eunit.hrl").

align(N, L) ->
  align(N, N, string:tokens(L, " \n\t"), []).

align(_, _, [], [R | Rs]) ->
  lists:reverse([lists:concat(lists:reverse(R)) | Rs]);

align(N, _, [L | _], _) when length(L) > N ->
  erlang:error(word_too_long_error);

%% first line
align(N, C, [L | Ls], []) ->
  align(N, C - length(L), Ls, [[L]]);

%% line continued
align(N, C, [L | Ls], [R | Rs]) when ((length(L) + 1) =< C) ->
  align(N, C - length(L) - 1, Ls, [[L, " " | R] | Rs]);

%% new line
align(N, _, [L | Ls], [R | Rs]) ->
  align(N, N - length(L), Ls, [[L] | [lists:concat(lists:reverse(R)) | Rs]]).

align_test() ->
  ["The heat bloomed in December as the",
    "carnival season kicked into gear.",
    "Nearly helpless with sun and glare,",
    "I avoided Rio's brilliant sidewalks",
    "and glittering beaches, panting in",
    "dark corners and waiting out the",
    "inverted southern summer."] = align(35, "The heat bloomed                  in December
 as the      carnival  season
                 kicked into gear.
Nearly helpless with sun and glare, I avoided Rio's brilliant
sidewalks
 and glittering beaches,
panting in dark     corners
  and waiting out the inverted southern summer.").


%% N - line width, L - list of lines
justify(N, L) ->
  justify(N, L, []).

justify(_, [], R) ->
  lists:reverse(R);

%% last line is to be left intact
justify(_, [L], R) ->
  lists:reverse([L | R]);

%% line needs justifying
justify(N, [L | Ls], R) when (length(L) < N) ->
  Spaces = count_spaces(L, 0),
  Missing = N - length(L),
  case Spaces of
    0 -> justify(N, Ls, [L | R]);
    _ -> justify(N, Ls, [fill(Missing, Missing / Spaces, L, []) | R])
  end;

%% line already at desired length
justify(N, [L | Ls], R) ->
  justify(N, Ls, [L | R]).



fill(_, _, [], R) -> lists:reverse(lists:flatten(R));

%% N - remaining spaces to be added, Ratio - how many spaces per existing space need to be distributed
fill(N, Ratio, [L | Ls], R) when ((L == 32) and (Ratio == trunc(Ratio))) ->
  T = trunc(Ratio),
  fill(N - Ratio, T, Ls, [repeat(T, " ") | R]);

fill(N, Ratio, [L | Ls], R) when (L == 32) ->
  T = trunc(Ratio),
  case T == N of
    true -> fill(N - T, Ratio, Ls, [repeat(T + 1, " ") | R]);
    false -> fill(N - T - 1, Ratio, Ls, [repeat(T + 2, " ") | R])
  end;

fill(N, Ratio, [L | Ls], R) ->
  fill(N, Ratio, Ls, [L | R]).

fill_test() ->
  "carnival  season  kicked into gear." = fill(2, 2 / 4, "carnival season kicked into gear.", []).


%% repeat string X N-times
repeat(N, X) -> lists:flatten(lists:duplicate(N, X)).

repeat_test() -> "   " = repeat(3, " ").



%% count spaces in the line
%% L - line, N - counter
count_spaces([], N) -> N;

count_spaces([L | Ls], N) when (L == 32) -> count_spaces(Ls, N + 1);

count_spaces([_ | Ls], N) -> count_spaces(Ls, N).

count_spaces_test() ->
  3 = count_spaces("   ", 0),
  2 = count_spaces("a b c", 0),
  2 = count_spaces("ab c ", 0).



justify_test() ->
  A = align(35, "The heat bloomed                  in December
 as the      carnival  season
                 kicked into gear.
Nearly helpless with sun and glare, I avoided Rio's brilliant
sidewalks
 and glittering beaches,
panting in dark     corners
  and waiting out the inverted southern summer."),
  ["The heat bloomed in December as the",
    "carnival  season  kicked into gear.",
    "Nearly helpless with sun and glare,",
    "I avoided Rio's brilliant sidewalks",
    "and  glittering beaches, panting in",
    "dark  corners  and  waiting out the",
    "inverted southern summer."] = justify(35, A).
