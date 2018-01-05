type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2val crazyinput =
  let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n ->
        let b = pow a (n / 2) in
        b * b * (if n mod 2 = 0 then 1 else a)
  in
  let rec subcrazy2val c i =
    match c with
    | NIL -> 0
    | ZERO cc -> subcrazy2val cc (i + 1)
    | ONE cc -> (subcrazy2val cc (i + 1)) + (pow 2 i)
    | MONE cc -> (subcrazy2val cc (i + 1)) - (pow 2 i)
  in
  subcrazy2val crazyinput 0

(* TEST CASE *)
(*
let _ =
  print_int (crazy2val (ZERO (ONE (MONE NIL))));
  print_int (crazy2val (ONE (MONE NIL)));
  print_int (crazy2val (ONE (ZERO (ONE NIL))))
;;

*)
