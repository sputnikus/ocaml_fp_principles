open Adt

let rec tree_depth = function
  | Leaf -> 0
  | Node (left, _, right) -> max (tree_depth left) (tree_depth right) + 1

let tree_to_list tree =
  let rec aux t acc =
    match t with
    | Leaf -> acc
    | Node (left, value, right) -> aux left (value :: aux right acc)
  in
  aux tree []

let rec find value = function
  | Leaf -> false
  | Node (left, v, right) ->
      if value = v then true
      else if value < v then find value left
      else find value right

let%test "tree depth simple" = tree_depth (Node (Leaf, 2, Leaf)) = 1

let%test "tree depth side loaded" =
  tree_depth
    (Node
       ( Node (Node (Node (Node (Leaf, 1, Leaf), 2, Leaf), 3, Leaf), 4, Leaf),
         5,
         Leaf ))
  = 5

let%test "tree to list" =
  tree_to_list
    (Node
       ( Node (Node (Node (Node (Leaf, 1, Leaf), 2, Leaf), 3, Leaf), 4, Leaf),
         5,
         Leaf ))
  = [ 1; 2; 3; 4; 5 ]

let%test "find present" =
  find 1
    (Node
       ( Node (Node (Node (Node (Leaf, 1, Leaf), 2, Leaf), 3, Leaf), 4, Leaf),
         5,
         Leaf ))
  = true

let%test "find missing" = find 1 Leaf = false
