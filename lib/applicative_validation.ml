open Sexplib.Std
open Ppx_compare_lib.Builtin

(* Applicative validation enables collecting multiple independent errors, rather
than failing at the first. Unlike monads (which short-circuit on the first
error), applicative validation accumulates all errors by combining them. *)
type ('err, 'a) vresult = Ok of 'a | Error of 'err [@@deriving sexp, compare]

(* Inject value into vresult *)
let vreturn x = Ok x

(* Apply function in context, accumulating errors *)
let vapply func x =
  match (func, x) with
  | Ok f, Ok x -> Ok (f x)
  | Error errs1, Error errs2 -> Error (errs1 @ errs2)
  | Error errs, Ok _ | Ok _, Error errs -> Error errs

(* Validate pair of applications *)
let vpair a b = vapply (vapply (vreturn (fun x y -> (x, y))) a) b

(* 3-ary applicative map *)
let vmap3 f a b c = vapply (vapply (vapply (vreturn f) a) b) c

let%test "both result succeed means final result succeeds" =
  let func = vreturn (fun x -> x + 1) in
  let x = vreturn 2 in
  vapply func x = Ok 3

let%test "one result fails means one error gets reported" =
  let func = vreturn (fun x -> x + 1) in
  let x = Error [ "error" ] in
  vapply func x = Error [ "error" ]

let%test "both results fail means all errors get reported" =
  let x = Error [ "error1" ] in
  let y = Error [ "error2" ] in
  vapply x y = Error [ "error1"; "error2" ]

let%test_unit "collect error from the chain of validations" =
  let validate_positive n = if n > 0 then Ok n else Error [ "not positive" ] in

  let validate_nonempty s =
    if s <> "" then Ok s else Error [ "empty string" ]
  in
  [%test_eq: (string list, int * string) vresult]
    (vpair (validate_positive 0) (validate_nonempty ""))
    (Error [ "not positive"; "empty string" ])

let%test_unit "succeed in the chain of validations" =
  let validate_positive n = if n > 0 then Ok n else Error [ "not positive" ] in

  let validate_nonempty s =
    if s <> "" then Ok s else Error [ "empty string" ]
  in
  [%test_eq: (string list, int * string) vresult]
    (vpair (validate_positive 42) (validate_nonempty "bar"))
    (Ok (42, "bar"))
