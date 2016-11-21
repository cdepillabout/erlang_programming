%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(serial).

-export([treeToList/1,treeToBin/1,tree1/0,binToTree/1,listToTree/1,test1/0,test2/0,test3/0, tree2/0]).

treeToList({leaf,N}) ->
    [2,N];
treeToList({node,T1,T2}) ->
    TTL1 = treeToList(T1),
    [Size1|_] = TTL1,
    TTL2 = treeToList(T2),
    [Size2|_] = TTL2,
    [Size1+Size2+1|TTL1++TTL2].

treeToBin({leaf, N}) ->
  io:format("in treeToBin leaf, N is: ~w~n", [N]),
  BinaryN = term_to_binary(N),
  BinaryNLen = bit_size(BinaryN),
  TotalLen = BinaryNLen + 24,
  <<0:8, TotalLen:16/integer, BinaryN:BinaryNLen/bits>>;
treeToBin({node, T1, T2}) ->
  io:format("in treeToBin node, T1 is: ~w, T2 is: ~w~n", [T1, T2]),
  TTL1 = treeToBin(T1),
  <<_TreeType1:8/integer, TreeBitSize1:16/integer, _TreeBinary1/bits>> = TTL1,
  TTL2 = treeToBin(T2),
  io:format("in treeToBin node, TTL1 is: ~w, TTL2 is: ~w~n", [TTL1, TTL2]),
  <<_TreeType2:8/integer, TreeBitSize2:16/integer, _TreeBinary2/bits>> = TTL2,
  NewSize = TreeBitSize1  + TreeBitSize2 + 24,
  << 1:8, NewSize:16/integer, TTL1/bits, TTL2/bits>>.

listToTree([2,N]) ->
  {leaf,N};
listToTree([_|Code]) ->
  case Code of
    [M|_] ->
      {Code1,Code2} = lists:split(M,Code),
      {node,
       listToTree(Code1),
       listToTree(Code2)
      }
  end.

binToTree(<<0:8/integer, _BinaryNLen:16/integer, BinaryN/bits>>) ->
  io:format("in binToTree, decoding leaf: _BinaryNLen: ~w, BinaryN: ~w~n", [_BinaryNLen, BinaryN]),
  {leaf, binary_to_term(BinaryN)};
binToTree(<<1:8/integer, _TotalSize:16/integer, Rest/binary>>) ->
  io:format("in binToTree, decoding node: _TotalSize: ~w, Rest: ~w~n", [_TotalSize, Rest]),
  <<Tree1Type:8/integer, Tree1TotalSize:16/integer, Temp/bits>> = Rest,
  io:format("in binToTree, decoding node: Tree1Type: ~w, Tree1TotalSize: ~w~n", [Tree1Type, Tree1TotalSize]),
  Tree1InfoSize = Tree1TotalSize - 24,
  io:format("in binToTree, decoding node: Tree1InfoSize: ~w~n", [Tree1InfoSize]),
  io:format("in binToTree, decoding node: Tree1InfoSize: ~w, Temp: ~w~n", [Tree1InfoSize, Temp]),
  <<Tree1Stuff:Tree1InfoSize/bits, Tree2/bits>> = Temp,
  io:format("in binToTree, decoding node: Tree2: ~w~n", [Tree2]),
  <<Tree2Type:8/integer, Tree2TotalSize:16/integer, Tree2Stuff/bits>> = Tree2,
  io:format("in binToTree, decoding node: Tree2Type: ~w, Tree2TotalSize: ~w, Tree2Info: ~w~n", [Tree2Type, Tree2TotalSize, Tree2Stuff]),
  {node,
   binToTree(<<Tree1Type:8/integer, Tree1TotalSize:16/integer, Tree1Stuff/bits>>),
   binToTree(<<Tree2Type:8/integer, Tree2TotalSize:16/integer, Tree2Stuff/bits>>)
  }.

tree1() ->
  {node,
   {node,
    {leaf,cat},
    {node,
     {leaf,dog},
     {leaf,emu}
    }
   },
   {leaf,fish}
  }.

tree2() ->
  {node,
   tree1(),
   tree1()
  }.

test1() ->
  treeToList(tree1()).

test2() ->
  treeToList(tree2()).

test3() ->
  tree1() == listToTree(treeToList(tree1())).
