-module(index).
-export([get_file_contents/1, show_file_contents/1]).
-include_lib("eunit/include/eunit.hrl").

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
  {ok, File} = file:open(Name, [read]),
  Rev = get_all_lines(File, []),
  lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File, Partial) ->
  case io:get_line(File, "") of
    eof -> file:close(File),
      Partial;
    Line -> {Strip, _} = lists:split(length(Line) - 1, Line),
      get_all_lines(File, [Strip | Partial])
  end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L | Ls]) ->
  io:format("~s~n", [L]),
  show_file_contents(Ls);
show_file_contents([]) ->
  ok.


%% My Implementation
%% ---------------------------------------------------------------------------------------------------------------------

%% Splits a list of lines into list of tuples:
%% {N, L}
%% where:
%% N - line number
%% L - list of words in the line (normalized, stripped of punctuation and filtered 3-letter words and longer)
split(L) ->
  split(L, 1, []).

split([], _, R) -> lists:reverse(R);

split([L | Ls], N, R) ->
  split(Ls, N + 1, [{N, normalize(filter(3, string:tokens(L, " .,\\-`'\"_!?")))} | R]).

split_test() ->
  [{1, ["but", "larger", "sense", "can", "not", "dedicate"]}] = split(["But, in a larger sense, we can not dedicate --"]),
  [] = split([]),
  [{1, []}] = split([""]),
  [{1, []}] = split([[]]),
  [{1, ["but", "larger", "sense"]}, {2, []}, {3, ["this", "cannot"]}] = split(["But, larger sense", [], "this cannot"]).


%% Normalizes list of words - changes upper into lower case
normalize(L) ->
  normalize(L, []).

normalize([], R) -> lists:reverse(R);

normalize([L | Ls], R) ->
  normalize(Ls, [normalize_str(L) | R]).

normalize_str([]) -> [];

normalize_str([S | Ss]) when ((S >= $a) andalso (S =< $z)) ->
  [S | normalize_str(Ss)];

normalize_str([S | Ss]) when ((S >= $A) andalso (S =< $Z)) ->
  [S + 32 | normalize_str(Ss)];

normalize_str([_ | Ss]) ->
  normalize_str(Ss).

normalize_test() ->
  ["but", "in", "a", "larger", "sense", "we", "can", "not", "dedicate"] = normalize(["But", "in", "a", "larger", "sense", "we", "can", "not", "dedicate"]),
  ["but", "in", "a", "larger", "sense", "we", "can", "not", "dedicate"] = normalize(["BUT", "IN", "A", "LARGER", "SENSE", "WE", "CAN", "NOT", "DEDICATE"]).


%% Filters list of words including only words longer or equal N
filter(N, L) when N > 0 ->
  filter(N, L, []).

filter(_, [], R) -> lists:reverse(R);

filter(N, [L | Ls], R) when length(L) >= N ->
  filter(N, Ls, [L | R]);

filter(N, [_ | Ls], R) ->
  filter(N, Ls, R).

filter_test() ->
  ["hovercraft", "full", "eel"] = filter(3, ["my", "hovercraft", "is", "full", "of", "eel"]),
  [] = filter(15, ["my", "hovercraft", "is", "full", "of", "eel"]),
  ["my", "hovercraft", "is", "full", "of", "eel"] = filter(1, ["my", "hovercraft", "is", "full", "of", "eel"]).


%% Creates a map of words and line numbers, where the word occurs. Returns:
%% #{W => L}
%% where:
%% W - word
%% L - list of tuples {N, N} denoting occurrences of given word (N - line number)
gather(L) ->
  gather(L, #{}).

gather([], R) -> R;

gather([{N, L} | Ls], R) ->
  gather(Ls, gather_from_line(N, L, R)).

%% Processes one line, collecting words into map
gather_from_line(_, [], R) -> R;

gather_from_line(N, [L | Ls], R) ->
  case maps:is_key(L, R) of
    true ->
      #{L := I} = R,
      gather_from_line(N, Ls, R#{L := join(I, [{N, N}])});
    false ->
      gather_from_line(N, Ls, R#{L => [{N, N}]})
  end.

%% Joins two lists together
join(X, Y) ->
  merge(lists:reverse(X), Y).

merge([], Y) -> Y;

merge([X | Xs], Y) ->
  merge(Xs, [X | Y]).

gather_test() ->
  #{"but" := [{1, 1}, {3, 3}, {4, 4}],
    "larger" := [{1, 1}],
    "sense" := [{1, 1}],
    "this" := [{3, 3}],
    "cannot" := [{3, 3}]} = gather([{1, ["but", "larger", "sense"]}, {2, []}, {3, ["but", "this", "cannot"]}, {4, ["but"]}]).


%% Compresses line numbers into ranges whenever a sequence of numbers is found for a word in a word map. Returns:
%% #{W => L}
%% where:
%% W - word
%% L - list of tuples {N, M} denoting occurrences of given word (N, M - line numbers, N <= M)
compress(M) ->
  maps:map(fun(K, V) -> compress_list(K, V) end, M).

compress_list(_, V) ->
  compress_numbers(V, {}, []).

%% Processes a list of tuples {N, N} and returns a compressed list (with ranges if such exist).
%% Takes:
%% L - list of tuples {N, N},
%% T - tuple {N, M} with temporary range that grows if next line is part of range,
%% R - accumulated list of processed tuples
compress_numbers([], _, R) -> lists:reverse(R);

compress_numbers([L], {}, R) -> lists:reverse([L | R]);

compress_numbers([L], {T1, _}, R) ->
  {Max, _} = L,
  lists:reverse([{T1, Max} | R]);

compress_numbers([L1, L2 | Ls], {T1, T2}, R) ->
  {L1_First, _} = L1,
  Next = L1_First + 1,
  {L2_First, _} = L2,
  case L2_First of
    Next -> compress_numbers([L2 | Ls], {T1, L2_First}, R);
    _ -> compress_numbers([L2 | Ls], {}, [{T1, T2} | R])
  end;

compress_numbers([L1, L2 | Ls], {}, R) ->
  {L1_First, _} = L1,
  Next = L1_First + 1,
  {L2_First, _} = L2,
  case L2_First of
    Next -> compress_numbers([L2 | Ls], {L1_First, L2_First}, R);
    _ -> compress_numbers([L2 | Ls], {L2_First, L2_First}, [L1 | R])
  end.

compress_test() ->
  #{"but" := [{1, 1}, {3, 4}],
    "larger" := [{1, 1}, {7, 7}],
    "sense" := [{1, 1}],
    "cannot" := [{3, 5}]} = compress(#{"but" => [{1, 1}, {3, 3}, {4, 4}], "larger" => [{1, 1}, {7, 7}], "sense" => [{1, 1}], "cannot" => [{3, 3}, {4, 4}, {5, 5}]}).


%% Calculates word occurrence stats in a given file. W - word, F - file name. Returns a tuple:
%% {W, L}
%% where:
%% W - word
%% L - list of tuples {N, M} denoting occurrences of given word (N, M - line numbers, N <= M)
stats(W, F) ->
  {W, maps:get(W, compress(gather(split(get_file_contents(F)))))}.

result_test() ->
  {"now", [{5, 5}]} = stats("now", "gettysburg-address.txt"),
  {"dedicated", [{3, 3}, {6, 6}, {19, 19}, {21, 21}]} = stats("dedicated", "gettysburg-address.txt"),
  {"mean", [{28, 28}, {154, 154}, {527, 527}, {2490, 2490}, {3030, 3030}, {3724, 3724}]} = stats("mean", "dickens-christmas.txt").


%% Potential improvements:
%%
%% - merge 'gather' and 'compress' into one operation to save on time complexity
%% - normalize forms of words, e.g. recognize singular and plural as the same word
%% - 'compress_numbers' could be refactored to some more concise form