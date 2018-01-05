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

