open Adt

let rec map_tree func = function
  | Leaf -> Leaf
  | Node (left, value, right) ->
      Node (map_tree func left, func value, map_tree func right)

let rec fold_tree op acc = function
  | Leaf -> acc
  | Node (left, value, right) ->
      let left_acc = fold_tree op acc left in
      let node_acc = op left_acc value in
      fold_tree op node_acc right

let%test "double the tree values" =
  map_tree
    (fun x -> x * 2)
    (Node (Node (Leaf, 3, Leaf), 5, Node (Leaf, 7, Leaf)))
  = Node (Node (Leaf, 6, Leaf), 10, Node (Leaf, 14, Leaf))

let%test "sum using fold" =
  fold_tree ( + ) 0 (Node (Node (Leaf, 3, Leaf), 5, Node (Leaf, 7, Leaf))) = 15
