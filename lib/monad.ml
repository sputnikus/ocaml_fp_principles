(* A monad is an abstraction that enables sequencing computations with context
(side effects, error, etc.). *)
open Functor

let return_tree value = Applicative.pure_tree value

(* Grafting helpers *)
let rec graft_left tree l_subtree =
  match tree with
  | Leaf -> l_subtree
  | Node (l, v, r) -> Node (graft_left l l_subtree, v, r)

let rec graft_right tree r_subtree =
  match tree with
  | Leaf -> r_subtree
  | Node (l, v, r) -> Node (l, v, graft_right r r_subtree)

(* Traverse the tree, replacing each value with a new subtree (and grafting
those together). *)
let rec bind_tree tree func =
  match tree with
  | Leaf -> Leaf
  | Node (left, value, right) ->
      let left' = bind_tree left func in
      let right' = bind_tree right func in
      let middle = func value in
      let left_side = graft_left middle left' in
      graft_right left_side right'

let%test "return_tree x |> bind_tree f = f x (left identity)" =
  let f x = return_tree (x + 1) in
  let tree = Node (Leaf, 2, Leaf) in
  let expected = Node (Leaf, 3, Leaf) in
  bind_tree tree f = expected

let%test "bind_tree t return_tree = t (right identity)" =
  let tree = Node (Leaf, 2, Leaf) in
  let expected = Node (Leaf, 2, Leaf) in
  bind_tree tree return_tree = expected

let%test
    "'flattening' by mapping each value to a tree and flattening the result." =
  let t = Node (Node (Leaf, 1, Leaf), 2, Leaf) in
  let f x = Node (Leaf, x, Node (Leaf, x + 10, Leaf)) in
  let result = bind_tree t f in
  (* For the above, you could check that the in-order traversal is as expected: *)
  result = Node (Node (Leaf, 1, Node (Leaf, 11, Leaf)), 2, Node (Leaf, 12, Leaf))
