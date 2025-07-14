(* A semigroup is a set equipped with an associative binary operation
(e.g., integer addition, string concatenation). In functional programming, it is
often encoded as a type plus a combine function satisfying associativity.*)

module type SEMIGROUP = sig
  type t

  val combine : t -> t -> t
end

module IntAdd : SEMIGROUP with type t = int = struct
  type t = int

  let combine = ( + )
end

module StringConcat : SEMIGROUP with type t = string = struct
  type t = string

  let combine = ( ^ )
end

(* Parametrise with a module that has just type and this type will be our list
element *)
module ListAppend (T : sig
  type t
end) : SEMIGROUP with type t = T.t list = struct
  type t = T.t list

  let combine = ( @ )
end

(* Combine semigroups should be tested for associativity *)
let%test "integer combine should be associative" =
  IntAdd.combine (IntAdd.combine 1 2) 3 = IntAdd.combine 1 (IntAdd.combine 2 3)

let%test "string combine should be associative" =
  StringConcat.combine (StringConcat.combine "a" "b") "c"
  = StringConcat.combine "a" (StringConcat.combine "b" "c")

let%test "list append should be associative" =
  let module Int = struct
    type t = int
  end in
  let module IntListAppend = ListAppend (Int) in
  let onetwothree = IntListAppend.combine [ 1; 2 ] [ 3 ] in
  let threefour = IntListAppend.combine [ 3 ] [ 4 ] in
  IntListAppend.combine onetwothree [ 4 ]
  = IntListAppend.combine [ 1; 2 ] threefour
