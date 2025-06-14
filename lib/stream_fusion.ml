open Lazy_streams

(* Applies a function to each stream element; *)
(* includes only elements for which the function returns Some *)
(* Should not build intermediate streams or lists *)
let rec lmap_filter f (LCons (h, tf)) =
  match f h with
  | None -> lmap_filter f (Lazy.force tf)
  | Some x -> LCons (x, lazy (lmap_filter f (Lazy.force tf)))

let%test "Take first 5 squares of even numbers and verify the result." =
  ltake 5
    (lmap
       (fun x -> x * x)
       (lmap_filter
          (fun x -> if x mod 2 = 0 then Some x else None)
          (lnaturals_from 1)))
  = [ 4; 16; 36; 64; 100 ]
