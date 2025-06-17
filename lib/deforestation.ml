(* This implementation allocates an intermediate list. *)
let squares_of_even lst =
  lst |> List.filter (fun x -> x mod 2 = 0) |> List.map (fun x -> x * x)

(* This implementation is single traversal. *)
let squares_of_even_deforested lst =
  List.fold_right
    (fun x acc -> if x mod 2 = 0 then (x * x) :: acc else acc)
    lst []

let%test "Identical result on ordinary case" =
  let test = [ 1; 2; 3; 4; 5 ] in
  squares_of_even test = squares_of_even_deforested test

let%test "Identical result on odd-only list" =
  let test = [ 1; 3; 5; 7; 9 ] in
  squares_of_even test = squares_of_even_deforested test
