(* Higher-order functions that combine small parsing functions into more complex
parsers. In functional programming, a parser is typically a function of type:
string -> (result * string) option
It consumes input, producing a result and remaining input, or fails (None).

You build complex parsers by combining primitives with combinators. *)
type 'a parser = string -> ('a * string) option

(* Parse a single given character *)
let char (c : char) =
 fun input ->
  match Seq.uncons (String.to_seq input) with
  | Some (c', rest) when c = c' -> Some (c, String.of_seq rest)
  | _ -> None

(* Always succeed without consuming input *)
let return x = fun input -> Some (x, input)

(* Monadic bind *)
let bind x func =
 fun input ->
  match x input with Some (value, rest) -> func value rest | None -> None

(* Functor map *)
let map func x =
 fun input ->
  match x input with
  | Some (value, rest) -> Some (func value, rest)
  | None -> None

(* Two parsers sequencer *)
let seq x y =
 fun input ->
  bind x (fun value1 -> bind y (fun value2 -> return (value1, value2))) input

let%test "char 'a' should succeed on \"abc\"" = char 'a' "abc" = Some ('a', "bc")

let%test "sequencing two char parsers should parse two characters" =
  seq (char 'a') (char 'b') "abc" = Some (('a', 'b'), "c")

(* Tests for return *)
let%test "return should wrap value without consuming input" =
  return 42 "test" = Some (42, "test")

let%test "return should work with empty input" =
  return "value" "" = Some ("value", "")

(* Tests for bind *)
let%test "bind should chain parsers successfully" =
  let parser = bind (char 'a') (fun c -> return (Char.uppercase_ascii c)) in
  parser "abc" = Some ('A', "bc")

let%test "bind should fail if first parser fails" =
  let parser = bind (char 'a') (fun c -> return (Char.uppercase_ascii c)) in
  parser "xyz" = None

let%test "bind should fail if second parser fails" =
  let parser = bind (char 'a') (fun _ -> char 'b') in
  parser "acd" = None

(* Testing monadicity (???) of bind *)
let%test "bind should satisfy left identity" =
  let value = 'x' in
  let f c = return (Char.uppercase_ascii c) in
  bind (return value) f "test" = f value "test"

let%test "bind should satisfy right identity" =
  let parser = char 'a' in
  bind parser return "abc" = parser "abc"

let%test "bind should satisfy associativity" =
  let parser = char 'a' in
  let f c = return (Char.uppercase_ascii c) in
  let g c = return (Char.lowercase_ascii c) in
  let left = bind (bind parser f) g in
  let right = bind parser (fun x -> bind (f x) g) in
  left "abc" = right "abc"
