open Adt

let max_option a b =
  match (a, b) with None, x | x, None -> x | Some x, Some y -> Some (max x y)

let rec max_tree_value = function
  | Leaf -> None
  | Node (left, value, right) ->
      let children_max =
        max_option (max_tree_value left) (max_tree_value right)
      in
      max_option (Some value) children_max

let tree_at_depth tree depth =
  let rec aux tree depth =
    match (tree, depth) with
    | Leaf, 0 -> []
    | Leaf, _ -> []
    | Node (_, value, _), 0 -> [ value ]
    | Node (left, _, right), d -> aux left (d - 1) @ aux right (d - 1)
  in
  aux tree depth

let%test "double the tree values" =
  max_tree_value
    (Node
       ( Node (Node (Node (Node (Leaf, 5, Leaf), 4, Leaf), 3, Leaf), 2, Leaf),
         1,
         Leaf ))
  = Some 5

let%test "tree at depth root" =
  tree_at_depth
    (Node
       ( Node (Node (Node (Node (Leaf, 5, Leaf), 4, Leaf), 3, Leaf), 2, Leaf),
         1,
         Leaf ))
    0
  = [ 1 ]

let%test "tree at max depth" =
  tree_at_depth
    (Node
       ( Node (Node (Node (Node (Leaf, 5, Leaf), 4, Leaf), 3, Leaf), 2, Leaf),
         1,
         Leaf ))
    4
  = [ 5 ]

let%test "tree at max depth both branches" =
  tree_at_depth
    (Node
       ( Node (Node (Leaf, 1, Leaf), 2, Leaf),
         3,
         Node (Leaf, 4, Node (Leaf, 5, Leaf)) ))
    2
  = [ 1; 5 ]

let%test "tree at max depth too depp" =
  tree_at_depth
    (Node
       ( Node (Node (Leaf, 1, Leaf), 2, Leaf),
         3,
         Node (Leaf, 4, Node (Leaf, 5, Leaf)) ))
    4
  = []
