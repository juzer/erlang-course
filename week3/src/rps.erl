-module(rps).

-include_lib("eunit/include/eunit.hrl").
-export([]).

%% ROCK - PAPER - SCISSORS game

beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper.

result(A, A) -> draw;

result(A, B) ->
  case beat(A) == B of
    true -> B;
    _ -> A
  end.

result_test() ->
  paper = result(rock, paper),
  paper = result(paper, rock),
  scissors = result(paper, scissors),
  scissors = result(scissors, paper),
  rock = result(scissors, rock),
  rock = result(rock, scissors),
  draw = result(scissors, scissors),
  draw = result(rock, rock),
  draw = result(paper, paper).



tournament(X, Y) -> lists:sum(lists:map(fun({A, B}) -> case result(A, B) of
                                                         A -> 1;
                                                         B -> -1;
                                                         draw -> 0
                                                       end end, lists:zip(X, Y))).

tournament_test() ->
  -1 = tournament([rock, rock, paper, paper], [rock, paper, scissors, rock]).
