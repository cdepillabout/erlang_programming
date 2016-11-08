
-module(binary_tree).
-export ([sum_tree/1]).
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

is_ordered(Tree) ->
  fold_tree(fun(_) -> {true, -1, 9999} end,
            fun(Integer, LeftMax, RightMax) -> is_ordered_tree(Integer, LeftMax, RightMax) end,
            Tree).

is_ordered_tree(_, {false, _, _}, _) ->
  false;
is_ordered_tree(_, _, {false, _, _}) ->
  false;
is_ordered_tree(Integer, {true, LeftMax, _}, {true, _, RightMin}) ->
  (LeftMax =< Integer) and (Integer =< RightMin).

-define(DEBUG_EXPR(Expr), io:format("~p = ~p~n", [??Expr, Expr])).

test() ->
  Tree1 = #tree_node{val = 3, left_tree = leaf, right_tree = leaf},
  Tree2 = #tree_node{val = 2, left_tree = leaf, right_tree = leaf},
  Tree3 = #tree_node{val = 10, left_tree = Tree2, right_tree = leaf},
  Tree4 = #tree_node{val = 20, left_tree = Tree3, right_tree = Tree1},
  ?DEBUG_EXPR(sum_tree(leaf)),
  ?DEBUG_EXPR(sum_tree(Tree4)),
  ?DEBUG_EXPR(max_tree(leaf)),
  ?DEBUG_EXPR(max_tree(Tree3)),
  ?DEBUG_EXPR(max_tree(Tree4)),
  ?DEBUG_EXPR(min_tree(leaf)),
  ?DEBUG_EXPR(min_tree(Tree3)),
  ?DEBUG_EXPR(min_tree(Tree4)).
