(*
	CSE / 2013-11426 / Im DongYeop
	Homework 2: Exercies 2
*)

type crazy2 = NIL
						| ZERO of crazy2
						| ONE of crazy2
						| MONE of crazy2

let rec result((total: int), (now: int), (cin: crazy2)): int =
	match cin with
	| NIL -> total
	| ZERO zcin -> result(total, now * 2, zcin)
	| ONE ocin -> result(total + now, now * 2, ocin)
	| MONE mcin -> result(total - now, now * 2, mcin)

let crazy2val(c: crazy2): int =
	result(0, 1, c)

let mtwo = ZERO(ONE(MONE NIL)) 
let one = ONE(NIL) 
let five = ONE(ZERO(ONE NIL)) 
let mone = ONE(MONE NIL) 
let mnine = ONE(MONE(ZERO(MONE NIL))) 
let zero = ZERO(ZERO(ZERO NIL)) 
let big1 = ZERO(ONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE(ONE NIL))))))))) 
let big2 = ONE(MONE(MONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE NIL))))))))) 

