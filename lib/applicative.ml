(* An applicative is a functional abstraction that allows you to apply functions
embedded in a context (such as an option, list, or tree) to values in
a context. *)
open Functor

(* Returns a singleton tree with that value *)
let pure_tree value = Node (Leaf, value, Leaf)

(* Given a tree of functions and a tree of values, apply each function at the
matching position to the value at the same position *)
let rec ap_tree tf tt =
  match (tf, tt) with
  | Node (l1, f, r1), Node (l2, x, r2) ->
      Node (ap_tree l1 l2, f x, ap_tree r1 r2)
  | _, _ -> Leaf

let%test "ap_tree applies functions pointwise (shape-wise) over trees." =
  let f = fun x -> x + 1 in
  let g = fun x -> x * 2 in
  let function_tree = Node (Leaf, f, Node (Leaf, g, Leaf)) in
  let value_tree = Node (Leaf, 2, Node (Leaf, 3, Leaf)) in
  let expected = Node (Leaf, 3, Node (Leaf, 6, Leaf)) in
  ap_tree function_tree value_tree = expected

let%test "pure_tree f |> ap_tree x gives same result as map_tree f x." =
  let f = fun x -> x + 1 in
  let value_tree = Node (Leaf, 2, Leaf) in
  pure_tree f
  |> (fun fun_tree -> ap_tree fun_tree value_tree)
  = map_tree f value_tree

let%test
    "if shape differs, result is as 'empty' as the most empty subtree (i.e., \
     Leaf dominates)." =
  let f = fun x -> x + 1 in
  let g = fun x -> x * 2 in
  let function_tree = Node (Leaf, f, Node (Leaf, g, Leaf)) in
  let value_tree = Node (Leaf, 2, Leaf) in
  let expected = Node (Leaf, 3, Leaf) in
  ap_tree function_tree value_tree = expected
