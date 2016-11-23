%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(index).

-export([index/1,processFile/1,prettyEntry/1]).
-export([accumulate/1,removeDuplicates/1,groupSuccessors/1,createRanges/1]).

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
      ok;
    Line ->
      processLine(Line,N),
      processLines(IoDevice,N+1)
  end.

-define(Punctuation,"(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").

processLine(Line,N) ->
  case re:split(Line,?Punctuation) of
    {ok,Words} ->
      processWords(Words,N) ;
    _ -> []
  end.

processWords(Words,N) ->
  case Words of
    [] -> ok;
    [Word|Rest] ->
      if
        length(Word) > 3 ->
          Normalise = string:to_lower(Word),
          ets:insert(indexTable,{{Normalise , N}});
        true -> ok
      end,
      processWords(Rest,N)
  end.

prettyIndex() ->
  case ets:first(indexTable) of
    '$end_of_table' ->
      ok;
    First  ->
      case First of
        {Word, N} ->
          IndexEntry = {Word, [N]}
      end,
      prettyIndexNext(First,IndexEntry)
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

prettyEntry(IndexEntry) ->
    % exercise
    ok.

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
