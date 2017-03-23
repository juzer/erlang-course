-module(rps2).
-export([play/1, echo/1, play_two/3, rock/1, no_repeat/1, const/1, enum/1, cycle/1, rand/1, val/1, tournament/2, least_freq/1, most_freq/1]).

-include_lib("eunit/include/eunit.hrl").

%
% play one strategy against another, for N moves.
%

play_two(StrategyL, StrategyR, N) ->
  play_two(StrategyL, StrategyR, [], [], N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

play_two(_, _, PlaysL, PlaysR, 0) ->
  Result = tournament(PlaysL, PlaysR),
  io:format("Tournament result: ~w~n", [Result]);

play_two(StrategyL, StrategyR, PlaysL, PlaysR, N) ->
  MoveL = StrategyL(PlaysR),
  MoveR = StrategyR(PlaysL),
  Result = result(MoveL, MoveR),
  io:format("Round result: ~w~n", [Result]),
  play_two(StrategyL, StrategyR, [MoveL | PlaysL], [MoveR | PlaysR], N - 1).



neg_result(R) when R == win -> lose;
neg_result(R) when R == lose -> win;
neg_result(_) -> draw.


%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
  io:format("Rock - paper - scissors~n"),
  io:format("Play one of rock, paper, scissors, ...~n"),
  io:format("... r, p, s, stop, followed by '.'~n"),
  play(Strategy, []).

% tail recursive loop for play/1

play(Strategy, Moves) ->
  {ok, P} = io:read("Play: "),
  Play = expand(P),
  case Play of
    stop ->
      io:format("Stopped~n");
    _ ->
      Result = result(Play, Strategy(Moves)),
      io:format("Result: ~p~n", [Result]),
      play(Strategy, [Play | Moves])
  end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form

expand(r) -> rock;
expand(p) -> paper;
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock, rock) -> draw;
result(rock, paper) -> lose;
result(rock, scissors) -> win;
result(paper, rock) -> win;
result(paper, paper) -> draw;
result(paper, scissors) -> lose;
result(scissors, rock) -> lose;
result(scissors, paper) -> win;
result(scissors, scissors) -> draw.

% result of a tournament

tournament(PlaysL, PlaysR) ->
  lists:sum(
    lists:map(fun outcome/1,
      lists:zipwith(fun result/2, PlaysL, PlaysR))).

outcome(win) -> 1;
outcome(lose) -> -1;
outcome(draw) -> 0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) ->
  rock;
enum(1) ->
  paper;
enum(2) ->
  scissors.

val(rock) ->
  0;
val(paper) ->
  1;
val(scissors) ->
  2.

% give the play which the argument beats.

beats(rock) ->
  scissors;
beats(paper) ->
  rock;
beats(scissors) ->
  paper.

%
% strategies.
%
echo([]) ->
  paper;
echo([Last | _]) ->
  Last.

rock(_) ->
  rock.



no_repeat([]) ->
  paper;
no_repeat([X | _]) ->
  case X of
    rock -> scissors;
    scissors -> paper;
    paper -> rock
  end.



const(Play) ->
  fun(_) -> Play end.

cycle(Xs) ->
  enum(length(Xs) rem 3).

cycle_test() ->
  rock = cycle([]),
  paper = cycle([paper]),
  scissors = cycle([paper, rock]),
  rock = cycle([paper, rock, rock]).

rand(_) ->
  enum(rand:uniform(3) - 1).



most_freq([]) -> paper;
most_freq(Xs) ->
  Hist = lists:foldl(fun(Elem, OldMap) -> maps:update_with(Elem, fun(V) -> V + 1 end, 1, OldMap) end,
    #{},
    Xs),
  {Move, _} = maps:fold(fun find_highest/3, {paper, 0}, Hist),
  Move.

find_highest(K, V, {_, N}) when V > N -> {K, V};
find_highest(_, _, {Move, N}) -> {Move, N}.

most_freq_test() ->
  paper = most_freq([paper, rock, paper]),
  paper = most_freq([paper, rock, scissors]),
  rock = most_freq([rock, rock, rock]),
  rock = most_freq([rock, paper, rock, paper, paper, rock, rock]).



least_freq([]) -> paper;
least_freq(Xs) ->
  Hist = lists:foldl(fun(Elem, OldMap) -> maps:update_with(Elem, fun(V) -> V + 1 end, 1, OldMap) end,
    #{},
    Xs),
  {Move, _} = maps:fold(fun find_lowest/3, {paper, -1}, Hist),
  Move.

find_lowest(K, V, {_, -1}) -> {K, V};
find_lowest(K, V, {_, N}) when V < N -> {K, V};
find_lowest(_, _, {Move, N}) -> {Move, N}.

least_freq_test() ->
  rock = least_freq([paper, rock, paper]),
  paper = least_freq([paper, rock, scissors]),
  rock = least_freq([rock, rock, rock]),
  paper = least_freq([rock, paper, rock, paper, paper, rock, rock]).