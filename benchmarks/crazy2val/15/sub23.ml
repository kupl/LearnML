(* 2011-10915 / 생명과학부/ 신지민 / Homework 2-1 *)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun cr ->
	match cr with
	|NIL -> 0
	|ZERO cr2 -> 0+ 2*crazy2val(cr2)
	|ONE cr2 -> 1+ 2*crazy2val(cr2)
	|MONE cr2 -> -1 + 2*crazy2val(cr2)

(*
let a = ONE(ZERO(ONE NIL))
let b = ONE(MONE NIL)
let c = ONE(MONE(ZERO(MONE NIL)))
let d = NIL


let a1 = crazy2val a
let b1 = crazy2val b
let c1 = crazy2val c
let d1 = crazy2val d

let _= print_endline(string_of_int a1) 
let _= print_endline(string_of_int b1)
let _= print_endline(string_of_int c1)
let _= print_endline(string_of_int d1)
*)

