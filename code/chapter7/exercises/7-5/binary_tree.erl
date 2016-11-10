
-module(binary_tree).
-export ([sum_tree/1, is_ordered/1]).
-export ([min3/3, max3/3]).
-export ([test/0]).

-record(tree_node, {val :: integer(),
                    left_tree :: tree(),
                    right_tree :: tree()}).

-type tree_node() :: #tree_node{}.
-type leaf() :: leaf.
-type tree() :: tree_node() | leaf().

-spec fold_tree(fun((leaf) -> B), fun((integer(), B, B) -> B), tree()) -> B.
fold_tree(LeafFold, _TreeNodeFold, leaf) ->
  LeafFold(leaf);
fold_tree(LeafFold, TreeNodeFold,
          #tree_node{val = Val, left_tree = LeftTree, right_tree = RightTree}) ->
  TreeNodeFold(Val,
               fold_tree(LeafFold, TreeNodeFold, LeftTree),
               fold_tree(LeafFold, TreeNodeFold, RightTree)).

-spec sum_tree(tree()) -> integer().
sum_tree(Tree) ->
  fold_tree(fun(_) -> 0 end,
            fun(Integer, LeftSum, RightSum) -> Integer + LeftSum + RightSum end,
            Tree).

max_tree(Tree) ->
  fold_tree(fun(_) -> -1 end,
            fun(Integer, LeftMax, RightMax) -> max(Integer, max(LeftMax, RightMax)) end,
            Tree).

min_tree(Tree) ->
  fold_tree(fun(_) -> 9999 end,
            fun(Integer, LeftMax, RightMax) -> min(Integer, min(LeftMax, RightMax)) end,
            Tree).

-spec min3(number(), number(), number()) -> number().
min3(A, B, C) -> min(A, min(B, C)).

-spec max3(number(), number(), number()) -> number().
max3(A, B, C) -> max(A, max(B, C)).

-spec is_ordered(tree()) -> {boolean(), integer(), integer()}.
is_ordered(Tree) ->
  fold_tree(fun(_) ->
                {true, none, none}
            end,
            fun(Integer, Left, Right) ->
                is_ordered_tree(Integer, Left, Right)
            end,
            Tree).

-spec is_ordered_tree(integer(), {boolean(), integer(), integer()}, {boolean(), integer(), integer()}) -> {boolean(), integer(), integer()}.
is_ordered_tree(_, {false, LeftMin, _}, {_, _, RightMax}) ->
  {false, LeftMin, RightMax};
is_ordered_tree(_, {_, LeftMin, _}, {false, _, RightMax}) ->
  {false, LeftMin, RightMax};
is_ordered_tree(Integer, {true, LeftMin, LeftMax}, {true, RightMin, RightMax}) ->
  {test_is_ordered(Integer, LeftMax, RightMin), findMin(Integer, LeftMin), findMax(Integer, RightMax)}.

-spec findMin(integer(), none | integer()) -> integer().
findMin(Integer, none) -> Integer;
findMin(_, LeftMin) -> LeftMin.

-spec findMax(integer(), none | integer()) -> integer().
findMax(Integer, none) -> Integer;
findMax(_, RightMax) -> RightMax.

-spec test_is_ordered(integer(), none | integer(), none | integer()) -> boolean().
test_is_ordered(_, none, none) -> true;
test_is_ordered(Integer, LeftMax, none) -> LeftMax =< Integer;
test_is_ordered(Integer, none, RightMin) -> Integer =< RightMin;
test_is_ordered(Integer, LeftMax, RightMin) -> (LeftMax =< Integer) and (Integer =< RightMin).

-define(DEBUG_EXPR(Expr), io:format("~p = ~p~n", [??Expr, Expr])).

test() ->
  Tree1 = #tree_node{val = 3, left_tree = leaf, right_tree = leaf},
  Tree2 = #tree_node{val = 2, left_tree = leaf, right_tree = leaf},
  Tree3 = #tree_node{val = 10, left_tree = Tree2, right_tree = leaf},
  Tree4 = #tree_node{val = 20, left_tree = Tree3, right_tree = Tree1},
  Tree5 = #tree_node{val = 4000, left_tree = leaf, right_tree = leaf},
  Tree6 = #tree_node{val = 20, left_tree = Tree3, right_tree = Tree5},
  ?DEBUG_EXPR(sum_tree(leaf)),
  ?DEBUG_EXPR(sum_tree(Tree4)),
  ?DEBUG_EXPR(max_tree(leaf)),
  ?DEBUG_EXPR(max_tree(Tree3)),
  ?DEBUG_EXPR(max_tree(Tree4)),
  ?DEBUG_EXPR(min_tree(leaf)),
  ?DEBUG_EXPR(min_tree(Tree3)),
  ?DEBUG_EXPR(min_tree(Tree4)),
  ?DEBUG_EXPR(is_ordered(leaf)),
  ?DEBUG_EXPR(is_ordered(Tree1)),
  ?DEBUG_EXPR(is_ordered(Tree3)),
  ?DEBUG_EXPR(is_ordered(Tree4)),
  ?DEBUG_EXPR(is_ordered(Tree6)),
  io:format("end~n").

