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
