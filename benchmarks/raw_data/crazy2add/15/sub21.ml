(* 2011-10915 / 생명과학부 / 신지민 / Homework 2-2 *)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun(c1,c2) ->
	match c1 with
	|NIL -> c2
	|ZERO tl1 -> 
		(match c2 with
		|NIL -> c1
		|ZERO tl2 -> ZERO(crazy2add(tl1,tl2))
		|ONE tl2 -> ONE(crazy2add(tl1,tl2))
		|MONE tl2 -> MONE(crazy2add(tl1,tl2))
		)
	|ONE tl1 ->
		(match c2 with
		|NIL -> c1
		|ZERO tl2 -> ONE(crazy2add(tl1,tl2))
		|ONE tl2 -> begin
			let tl = crazy2add(ONE NIL, tl1) in
			ZERO(crazy2add(tl,tl2))
			    end
		|MONE tl2 -> ZERO(crazy2add(tl1, tl2))
		)
	|MONE tl1 ->
		(match c2 with
		|NIL -> c1
		|ZERO tl2 -> MONE(crazy2add(tl1,tl2))
		|ONE tl2 -> ZERO(crazy2add(tl1,tl2))
		|MONE tl2 -> begin
			let tl = crazy2add(MONE NIL, tl1) in
			ZERO(crazy2add(tl, tl2))
			     end
		)
(*
let rec crazy2val : crazy2 -> int = fun cr ->
	match cr with
	|NIL -> 0
	|ZERO cr2 -> 0+ 2*crazy2val(cr2)
	|ONE cr2 -> 1+ 2*crazy2val(cr2)
	|MONE cr2 -> -1 + 2*crazy2val(cr2)


let a = ONE(ZERO(ONE NIL))
let b = ONE(MONE NIL)
let c = ONE(MONE(ZERO(MONE NIL)))
let d = NIL

let a1 = crazy2val a
let b1 = crazy2val b
let c1 = crazy2val c
let d1 = crazy2val d

let e = crazy2val(crazy2add(a,b))
let e1 = a1+b1
let _= print_endline(string_of_int e)
let _= print_endline(string_of_int e1)

let _= print_endline("na")

let e = crazy2val(crazy2add(c,d))
let e1 = c1+d1
let _= print_endline(string_of_int e)
let _= print_endline(string_of_int e1)

*)


