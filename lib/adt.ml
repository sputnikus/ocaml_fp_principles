type int_tree =
  | Leaf
  | Node of int_tree * int * int_tree

let rec sum_tree = function
  | Leaf -> 0
  | Node (left, value, right) -> sum_tree left + value + sum_tree right

let rec insert new_value = function
  | Leaf -> Node (Leaf, new_value, Leaf)
  | Node (left, value, right) when new_value > value -> Node (left, value, (insert new_value right))
  | Node (left, value, right) when new_value < value -> Node ((insert new_value left), value, right)
  | x -> x

let%test "summation" =
  (sum_tree
    (Node (
      Node (Leaf, 3, Leaf),
      5,
      Node (Leaf, 7, Leaf)
      ))
  ) = 15
  ;;

let%test "insertion left" =
  insert 3 (Node (Leaf, 5, Leaf)) = (Node (Node (Leaf, 3, Leaf), 5, Leaf))
;;

let%test "insertion right" =
  insert 7 (Node (Leaf, 5, Leaf)) = (Node (Leaf, 5, Node (Leaf, 7, Leaf)))
;;

let%test "insertion no duplicates" =
  insert 5 (Node (Leaf, 5, Leaf)) = (Node (Leaf, 5, Leaf))
;;

let%test "insertion empty tree" =
  insert 3 Leaf = (Node (Leaf, 3, Leaf))
;;
