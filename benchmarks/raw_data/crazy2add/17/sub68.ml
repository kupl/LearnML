type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

(** crazy2add: crazy2 * crazy2 -> crazy2 *)
let rec crazy2add (x,y) = match x with
  | NIL -> y
  | ZERO a -> (match y with
    | NIL -> x
    | ZERO b -> ZERO (crazy2add(a,b))  (* 0a + 0b = 0(a+b)*)
    | ONE b -> ONE (crazy2add(a,b))    (* 0a + 1b = 1(a+b)*)
    | MONE b -> MONE (crazy2add(a,b))  (* 0a + -1b = -1(a+b)*)
    )
  | ONE a -> (match y with
    | NIL -> x
    | ZERO b -> ONE (crazy2add(a,b)) (* 1a + 0b = 1(a+b)*)
    | ONE b -> ZERO (crazy2add(ONE NIL,crazy2add(a,b))) (* 1a + 1b = 0(1+(a+b))*)
    | MONE b -> ZERO (crazy2add(a,b)) (* 1a + -1b = 0(a+b)*)
    )
  | MONE a -> (match y with
    | NIL -> x
    | ZERO b -> MONE (crazy2add(a,b)) (* -1a + 0b = -1(a+b)*)
    | ONE b -> ZERO (crazy2add(a,b))  (* -1a + 1b = 0(a+b)*)
    | MONE b -> ZERO (crazy2add(MONE NIL,crazy2add(a,b))) (* -1a + -1b = 0(-1+(a+b))*)
    )

(** Testcases *)
(**
let rec crazy2val: crazy2 -> int = fun x ->
  match x with
  | NIL -> 0
  | ZERO y -> 2 * crazy2val(y)
  | ONE y -> 1 + 2 * crazy2val(y)
  | MONE y -> -1 + 2 * crazy2val(y)
;;

let _=
let print_bool x = print_endline (string_of_bool x) in

print_bool (0 = (crazy2val (crazy2add (ZERO NIL, ZERO NIL))));
print_bool (0 = (crazy2val (crazy2add (MONE NIL, ONE NIL))));
print_bool (1 = (crazy2val (crazy2add (ZERO NIL, ONE NIL))));
print_bool (4 = (crazy2val (crazy2add (ONE (ONE NIL), ONE NIL))));
print_bool (-683 = (crazy2val (crazy2add (MONE (ZERO (ZERO (ZERO NIL))), (ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL)))))))))))))))
;;
*)
