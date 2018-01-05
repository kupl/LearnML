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

let rec crazy2add (c1, c2) =
  let rec crazy2addsub c1 c2 carry =
  match c1, c2, carry with
  | NIL, _, 0 -> c2
  | NIL, _, 1 -> crazy2addsub (ONE NIL) c2 0
  | NIL, _, (-1) -> crazy2addsub (MONE NIL) c2 0
  | _, NIL, 0 -> c1
  | _, NIL, 1 -> crazy2addsub (ONE NIL) c1 0
  | _, NIL, (-1) -> crazy2addsub (MONE NIL) c1 0
  | ZERO cc1, ZERO cc2, 0 -> ZERO (crazy2addsub cc1 cc2 0)
  | ZERO cc1, ZERO cc2, 1 -> ONE (crazy2addsub cc1 cc2 0)
  | ZERO cc1, ZERO cc2, (-1) -> MONE (crazy2addsub cc1 cc2 0)
  | ZERO cc1, ONE cc2, 0 -> ONE (crazy2addsub cc1 cc2 0)
  | ZERO cc1, ONE cc2, 1 -> ZERO (crazy2addsub cc1 cc2 1)
  | ZERO cc1, ONE cc2, (-1) -> ZERO (crazy2addsub cc1 cc2 0)
  | ZERO cc1, MONE cc2, 0 -> MONE (crazy2addsub cc1 cc2 0)
  | ZERO cc1, MONE cc2, 1 -> ZERO (crazy2addsub cc1 cc2 0)
  | ZERO cc1, MONE cc2, (-1) -> ZERO (crazy2addsub cc1 cc2 (-1))
  | ONE cc1, ZERO cc2, 0 -> ONE (crazy2addsub cc1 cc2 0)
  | ONE cc1, ZERO cc2, 1 -> ZERO (crazy2addsub cc1 cc2 1)
  | ONE cc1, ZERO cc2, (-1) -> ZERO (crazy2addsub cc1 cc2 0)
  | ONE cc1, ONE cc2, 0 -> ZERO (crazy2addsub cc1 cc2 1)
  | ONE cc1, ONE cc2, 1 -> ONE (crazy2addsub cc1 cc2 1)
  | ONE cc1, ONE cc2, (-1) -> ONE (crazy2addsub cc1 cc2 0)
  | ONE cc1, MONE cc2, 0 -> ZERO (crazy2addsub cc1 cc2 0)
  | ONE cc1, MONE cc2, 1 -> ONE (crazy2addsub cc1 cc2 0)
  | ONE cc1, MONE cc2, (-1) -> MONE (crazy2addsub cc1 cc2 0)
  | MONE cc1, ZERO cc2, 0 -> MONE (crazy2addsub cc1 cc2 0)
  | MONE cc1, ZERO cc2, 1 -> ZERO (crazy2addsub cc1 cc2 0)
  | MONE cc1, ZERO cc2, (-1) -> ZERO (crazy2addsub cc1 cc2 (-1))
  | MONE cc1, ONE cc2, 0 -> ZERO (crazy2addsub cc1 cc2 0)
  | MONE cc1, ONE cc2, 1 -> ONE (crazy2addsub cc1 cc2 0)
  | MONE cc1, ONE cc2, (-1) -> MONE (crazy2addsub cc1 cc2 0)
  | MONE cc1, MONE cc2, 0 -> ZERO (crazy2addsub cc1 cc2 (-1))
  | MONE cc1, MONE cc2, 1 -> MONE (crazy2addsub cc1 cc2 0)
  | MONE cc1, MONE cc2, (-1) -> MONE (crazy2addsub cc1 cc2 (-1))
  | _ -> NIL
  in
  crazy2addsub c1 c2 0

(* TEST CASE *)
  (*
let _ =
  print_int (crazy2val (ZERO (ONE (MONE NIL))));
  print_int (crazy2val (ONE (MONE NIL)));
  print_int (crazy2val (ONE (ZERO (ONE NIL))));
  print_endline "";
;;

let _ =
  let z1 = (ZERO (ONE (MONE NIL))) in
  let z2 = (ONE (MONE NIL)) in
  let z3 = (ONE (ZERO (ONE NIL))) in
  let z4 = (ONE (ONE (ZERO (ZERO (MONE (ONE (MONE (ONE (ZERO (MONE NIL)))))))))) in
  let z5 = (ONE (MONE (ONE (ONE (MONE (MONE (ZERO (ONE (ONE (MONE (ONE NIL))))))))))) in
  print_int (crazy2val z3); print_endline "";
  print_int (crazy2val z4); print_endline "";
  print_int (crazy2val z5); print_endline "";
  print_int (crazy2val (crazy2add (z3, z4))); print_endline "";
  print_int (crazy2val (crazy2add (z5, z4))); print_endline "";
  print_int (crazy2val (crazy2add (z4, z5))); print_endline "";

  *)
