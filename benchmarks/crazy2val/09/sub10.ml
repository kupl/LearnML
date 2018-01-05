(* 2006-11782 Song Young-chan, Hw1-6 crazy2val *)

exception Error of string

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val crazy_number = 
	match crazy_number with
	 NIL -> raise (Error "invalid arg")
	|(ONE NIL) -> 1
	|(ZERO NIL) -> 0
	|(MONE NIL) -> -1
	|(ONE (remain)) -> 1 + 2*(crazy2val remain)
	|(ZERO (remain)) -> 0 + 2*(crazy2val remain)
	|(MONE (remain)) -> -1 + 2*(crazy2val remain)
