(* A functor in the functional programming sense is a type constructor that
supports a mapping operation. *)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec map_tree f = function
  | Leaf -> Leaf
  | Node (l, x, r) -> Node (map_tree f l, f x, map_tree f r)

let%test "identity mapping returns identical empty tree" =
  map_tree (fun x -> x) Leaf = Leaf

let%test "identity mapping returns identical root tree" =
  map_tree (fun x -> x) (Node (Leaf, 1.0, Leaf)) = Node (Leaf, 1.0, Leaf)

let%test
    "mapping function chain equals to mapping each function call on resulting \
     tree" =
  let left = map_tree (fun x -> float_of_int x *. 2.0) (Node (Leaf, 1, Leaf)) in
  let right =
    map_tree (fun x -> float_of_int x) (Node (Leaf, 1, Leaf))
    |> map_tree (fun y -> y *. 2.0)
  in
  left = right

let%test "identity mapping returns identical stream" =
  Streams.take 5 (Streams.map_stream (fun x -> x) (Streams.naturals_from 1))
  = [ 1; 2; 3; 4; 5 ]

let%test
    "mapping function chain equals to mapping each function call resulting \
     stream" =
  let left =
    Streams.take 5
      (Streams.map_stream
         (fun x -> float_of_int x *. 2.0)
         (Streams.naturals_from 1))
  in
  let right =
    Streams.map_stream (fun x -> float_of_int x) (Streams.naturals_from 1)
    |> Streams.map_stream (fun y -> y *. 2.0)
    |> Streams.take 5
  in
  left = right
