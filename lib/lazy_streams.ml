type 'a lazy_stream = LCons of 'a * 'a lazy_stream Lazy.t

(* Takes the first n elements from a lazy stream. *)
let rec ltake n (LCons (h, tf)) =
  if n <= 0 then []
  else if n = 1 then [ h ]
  else h :: ltake (n - 1) (Lazy.force tf)

(* Maps a function over a lazy stream. *)
let rec lmap f (LCons (h, (lazy tf))) = LCons (f h, lazy (lmap f tf))

(* Produces an infinite lazy stream of consecutive natural numbers starting from given n. *)
let rec lnaturals_from n = LCons (n, lazy (lnaturals_from (n + 1)))

let%test "Take first ten natural numbers" =
  ltake 10 (lnaturals_from 1) = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

let%test "Take minus ten natural numbers" = ltake (-1) (lnaturals_from 1) = []

let%test "Take first five natural numbers squared" =
  ltake 5 (lmap (fun x -> x * x) (lnaturals_from 1)) = [ 1; 4; 9; 16; 25 ]

let%test "Test true laziness" =
  let counter = ref 0 in
  let rec test_stream n =
    LCons
      ( n,
        lazy
          (incr counter;
           (* Side effect: increments counter only when forced *)
           test_stream (n + 1)) )
  in
  (* Only the first element is forced here: counter remains 0 *)
  let _ = ltake 1 (test_stream 0) in
  (* Now force 10 elements: counter should be incremented 9 times *)
  let _ = ltake 10 (test_stream 0) in
  !counter = 9
