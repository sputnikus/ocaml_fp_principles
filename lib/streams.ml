(* Each stream node contains a value and a suspended computation for the next node. *)
type 'a stream = Cons of 'a * (unit -> 'a stream)

(* Takes the first n elements from a stream. *)
let rec take n (Cons (h, tf)) = if n <= 0 then [] else h :: take (n - 1) (tf ())

(* Maps a function over a stream. *)
let rec map_stream f (Cons (h, tf)) = Cons (f h, fun () -> map_stream f (tf ()))

(* Produces an infinite stream of consecutive natural numbers starting from given n. *)
let rec naturals_from n = Cons (n, fun () -> naturals_from (n + 1))

let%test "Take first ten natural numbers" =
  take 10 (naturals_from 1) = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

let%test "Take minus ten natural numbers" = take (-1) (naturals_from 1) = []

let%test "Take first five natural numbers squared" =
  take 5 (map_stream (fun x -> x * x) (naturals_from 1)) = [ 1; 4; 9; 16; 25 ]
