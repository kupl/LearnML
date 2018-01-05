(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_first_homework\Exercise6.ml *)

(* Computer S&E/2007-15612/park sungjun*)
(*Exercise 6*)
exception Error of string 
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2 ;;
let rec crazy2val num=
let rec crazy2 n=
  match n with
  |NIL -> 0
  |ZERO x -> 0 + 2*(crazy2 (x))
  |ONE x -> 1 + 2*(crazy2 (x))
  |MONE x -> -1 + 2*(crazy2 (x))
in
if num=NIL then raise ( Error "Illegal input")
else (crazy2 num)
;;

