open Adt
open Pattern_matching

let make_tree_finder value = fun tree -> find value tree
let make_depth_limiter depth = fun tree -> depth >= tree_depth tree

let%test "tree finder for existing value" =
  let find5 = make_tree_finder 5 in
  find5 (Node (Node (Leaf, 3, Leaf), 5, Node (Leaf, 7, Leaf))) = true

let%test "tree finder for missing value" =
  let find5 = make_tree_finder 10 in
  find5 (Node (Node (Leaf, 3, Leaf), 5, Node (Leaf, 7, Leaf))) = false

let%test "depth limiter shallow" =
  let limit1 = make_depth_limiter 1 in
  limit1 (Node (Node (Leaf, 3, Leaf), 5, Node (Leaf, 7, Leaf))) = false

let%test "depth limiter deep" =
  let limit10 = make_depth_limiter 10 in
  limit10 (Node (Node (Leaf, 3, Leaf), 5, Node (Leaf, 7, Leaf))) = true
