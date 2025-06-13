open Adt

let rec replace_tree_value orig repl = function
  | Leaf -> Leaf
  | Node (left, value, right) ->
      if value = orig then
        Node
          ( replace_tree_value orig repl left,
            repl,
            replace_tree_value orig repl right )
      else
        Node
          ( replace_tree_value orig repl left,
            value,
            replace_tree_value orig repl right )

let%test "Replacement when value exists multiple times." =
  let tree = Node (Node (Leaf, 2, Leaf), 3, Node (Leaf, 2, Leaf)) in
  let expected = Node (Node (Leaf, 4, Leaf), 3, Node (Leaf, 4, Leaf)) in
  let actual = replace_tree_value 2 4 tree in
  actual = expected && tree <> expected

let%test "No-op when value does not exist." =
  let tree = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 2, Leaf)) in
  let expected = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 2, Leaf)) in
  let actual = replace_tree_value 3 4 tree in
  actual = expected && tree != actual

let%test "Replacement at root." =
  let tree = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)) in
  let expected = Node (Node (Leaf, 1, Leaf), 4, Node (Leaf, 3, Leaf)) in
  let actual = replace_tree_value 2 4 tree in
  actual = expected && tree <> expected
