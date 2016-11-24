%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(index).

-export([index/1,processFile/1,prettyEntry/1]).
-export([accumulate/1,removeDuplicates/1,groupSuccessors/1,createRanges/1,pad/2,prettyList/1]).

index(File) ->
  ets:new(indexTable, [ordered_set, named_table]),
  processFile(File),
  prettyIndex().

processFile(File) ->
  {ok,IoDevice} = file:open(File,[read]),
  processLines(IoDevice,1).

processLines(IoDevice,N) ->
  case io:get_line(IoDevice,"") of
    eof ->
      io:format("processLines: got EOF!~n"),
      ok;
    Line ->
      io:format("processLines: got line: ~s", [Line]),
      processLine(Line,N),
      processLines(IoDevice,N+1)
  end.

-define(Punctuation,"(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").

processLine(Line,N) ->
  Words = doRegex(Line,?Punctuation),
  io:format("processLine: got words: ~w!~n", [Words]),
  processWords(Words,N).

doRegex(String,Regex) ->
  RegexRes = re:split(String, Regex),
  lists:map(fun unicode:characters_to_list/1, RegexRes).

processWords(Words,N) ->
  case Words of
    [] -> ok;
    [Word|Rest] ->
      if
        length(Word) > 3 ->
          Normalise = string:to_lower(Word),
          io:format("processWords: inserting word: ~w~n", [Normalise]),
          ets:insert(indexTable,{{Normalise , N}});
        true -> ok
      end,
      processWords(Rest,N)
  end.

prettyIndex() ->
  case ets:first(indexTable) of
    '$end_of_table' ->
      ok;
    {Word,N} ->
      IndexEntry = {Word, [N]},
      prettyIndexNext({Word, N}, IndexEntry)
  end.


prettyIndexNext(Entry,{Word, Lines}=IndexEntry) ->
  Next = ets:next(indexTable,Entry),
  case Next of
    '$end_of_table' ->
      prettyEntry(IndexEntry);
    {NextWord, M}  ->
      if
        NextWord == Word ->
          prettyIndexNext(Next,{Word, [M|Lines]});
        true ->
          prettyEntry(IndexEntry),
          prettyIndexNext(Next,{NextWord, [M]})
      end
  end.

prettyEntry({Word, Lines}) ->
  % io:format("prettyEntry: Word: ~s, Lines: ~w, accumulate(Lines): ~w~n", [Word, Lines, accumulate(Lines)]),
  % io:format("prettyEntry: prettyList(accumulate(Lines)): ~s~n", [prettyList(accumulate(Lines))]),
  Index = pad(2, prettyList(accumulate(Lines))),
  io:format("~s~n~s~n", [Word, Index]).

accumulate(List) ->
  SortedList = lists:sort(List),
  NoDupsList = removeDuplicates(SortedList),
  GroupedList = groupSuccessors(NoDupsList),
  createRanges(GroupedList).

removeDuplicates([]) -> [];
removeDuplicates([H]) -> [H];
removeDuplicates([H1,H1|T]) ->
  removeDuplicates([H1|T]);
removeDuplicates([H1,H2|T]) ->
  [ H1 | removeDuplicates([H2|T]) ].

groupSuccessors(List) ->
  groupSuccessorsHelper(List, [], []).

groupSuccessorsHelper([ListHead|ListTail],
                      [],
                      FinalList) ->
  groupSuccessorsHelper(ListTail, [ListHead], FinalList);
groupSuccessorsHelper([ListHead|ListTail],
                      [SuccListHead|_]=SuccList,
                      FinalList) when SuccListHead + 1 == ListHead ->
  groupSuccessorsHelper(ListTail, [ListHead | SuccList], FinalList);
groupSuccessorsHelper([ListHead|ListTail],
                      SuccList,
                      FinalList) ->
  groupSuccessorsHelper(ListTail, [ListHead], FinalList ++ [lists:reverse(SuccList)]);
groupSuccessorsHelper([],
                      SuccList,
                      FinalList) ->
  FinalList ++ [lists:reverse(SuccList)].

createRanges(List) -> lists:map(fun createRangesHelper/1, List).

createRangesHelper([H]) -> {H};
createRangesHelper([H1,H2]) -> {H1,H2};
createRangesHelper([H1,_|T]) -> createRangesHelper([H1|T]).

prettyList(List) ->
  WithSep = lists:map(fun ({Item}) ->
                          integer_to_list(Item);
                          ({ItemA, ItemB}) ->
                          integer_to_list(ItemA) ++ "-" ++ integer_to_list(ItemB)
                      end,
                      List),
  Joined = string:join(WithSep, ","),
  Joined ++ ".".

pad(N, Word) when N > 0 ->
  pad(N - 1, " " ++ Word);
pad(_, Word) ->
  Word.
