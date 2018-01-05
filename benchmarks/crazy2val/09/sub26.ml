(* 
  Student no. : 2009-20769 
  Name : Kim, Seongjun 
*)
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

(*
let rec crazy2_of_string s =
  match s with
      [] -> NIL
    | '+' :: ss -> ONE (crazy2_of_string ss)
    | '-' :: ss -> MONE (crazy2_of_string ss)
    | '0' :: ss -> ZERO (crazy2_of_string ss)
    | _ -> NIL

;;
crazy2val (crazy2_of_string []);;   (* Exception *)
crazy2val (crazy2_of_string ['0']);; (* 0 *)
crazy2val (crazy2_of_string ['+']);; (* 1 *)
crazy2val (crazy2_of_string ['-']);; (* -1 *)
crazy2val (crazy2_of_string ['+';'+';'+';'+']);;   (* 15 *)
crazy2val (crazy2_of_string ['-';'-';'-';'-']);; (* -15 *)
crazy2val (crazy2_of_string ['0';'0';'0';'0']);; (* 0 *)
crazy2val (crazy2_of_string ['0';'+';'-';'+']);; (* 6 *)
*)
