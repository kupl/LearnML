
(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)

(* 6.ml *)
exception Error of string

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2val c =
  let rec c2v c =
    match c with
        NIL -> 0
      | ZERO c2 -> 2 * (c2v c2)
      | ONE c2 -> 1 + (2 * (c2v c2))
      | MONE c2 -> -1 + (2 * (c2v c2))
  in

    if c = NIL then
      raise (Error "value of NIL is not defined")
    else
      c2v c
(* 6.ml end *)

let crazy2add (x, y) =
  let trim_zero a =
    if a = (ZERO NIL) then
      ZERO NIL
    else 
      ZERO a
  in

  let rec c2a (x, y) =
    match (x, y) with 
        _, NIL -> x
      | NIL, _ -> y
      | ZERO xs, ONE ys | ONE xs, ZERO ys -> ONE (c2a (xs, ys))
      | ZERO xs, MONE ys | MONE xs, ZERO ys -> MONE (c2a (xs, ys))

      | ONE xs, ONE ys -> trim_zero (c2a ((c2a (xs, (ONE NIL))), ys))
      | MONE xs, MONE ys -> trim_zero (c2a ((c2a (xs, (MONE NIL))), ys))
      | ONE xs, MONE ys | MONE xs, ONE ys | ZERO xs, ZERO ys -> trim_zero (c2a (xs, ys))

  in
    match (x, y) with
	_, NIL | NIL, _ -> raise (Error "NIL is not valid")
      | _, _ -> c2a (x, y)


(*
let rec crazy2_of_string s =
  match s with
      [] -> NIL
    | '+' :: ss -> ONE (crazy2_of_string ss)
    | '-' :: ss -> MONE (crazy2_of_string ss)
    | '0' :: ss -> ZERO (crazy2_of_string ss)
    | _ -> NIL

;;
let z1 = crazy2_of_string [];; 
let z2 = crazy2_of_string ['+'];; 
let z3 = crazy2_of_string ['-'];;
let z4 = crazy2_of_string ['0'];; 
let z5 = crazy2_of_string ['+';'+'];; 
let z6 = crazy2_of_string ['-';'-'];; 
let z7 = crazy2_of_string ['0';'0';'0'];; 
let z8 = crazy2_of_string ['-';'+';'-'];; 
let z9 = crazy2_of_string ['+';'-';'+'];;
 
crazy2add(z1,z1);;      (* Exception *)
crazy2add(z2,z2);;      (* 0+ *)
crazy2add(z3,z3);;      (* 0- *)
crazy2add(z4,z4);;      (* 0 *)
crazy2add(z2,z3);;      (* 0 *)
crazy2add(z5,z5);;      (* 0++ *)
crazy2add(z6,z6);;      (* 0-- *)
crazy2add(z7,z7);;      (* 0 *)
crazy2add(z8,z9);;      (* 0 *)
*)
