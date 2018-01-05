type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun c -> 
	let rec cval : crazy2 * int -> int = fun (c2, i) ->
		let rec pow : int * int -> int = fun(a, x) ->
			if x == 0 then 1
			else pow(a, x-1) * a
		in	match c2 with
		| NIL -> 0
		| ZERO x -> cval(x, i+1)
		| ONE x -> cval(x, i+1) + pow (2,i)
		| MONE x -> cval(x, i+1) - pow(2,i)
	in cval(c, 0)
