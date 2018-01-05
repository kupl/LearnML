type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

(** crazy2val: crazy2 -> int *)
let rec crazy2val x = match x with
  | NIL -> 0
  | ZERO y -> 2 * crazy2val(y)
  | ONE y -> 1 + 2 * crazy2val(y)
  | MONE y -> -1 + 2 * crazy2val(y)

(** Testcases *)
(**
let _=
let print_bool x = print_endline (string_of_bool x) in
print_bool (-1  = (crazy2val (MONE NIL)));
print_bool (1   = (crazy2val (ONE (ZERO (ZERO (ZERO NIL))))));
print_bool (1   = (crazy2val (ONE NIL)));
print_bool (9   = (crazy2val (MONE (MONE (ONE (ONE (ZERO NIL)))))));
print_bool (-13 = (crazy2val (MONE (ZERO (ONE (ZERO (ONE (MONE NIL))))))))
;;
*)
