(* 2012-11230 Kim sangmin *)

type crazy2 = NIL
			| ZERO of crazy2
			| ONE of crazy2
			| MONE of crazy2

let rec crazy2val : crazy2 -> int = fun x ->
	let rec doubling : int*int -> int = fun (x, result) ->
		if(x=0) then result
		else doubling(x-1, 2*result)
	in
	let rec helper : crazy2*int*int -> int = fun (crazy,k,result) ->
		match crazy with
		| NIL -> result
		| ZERO(i) -> helper(i, k+1, result)
		| ONE(i) -> helper(i, k+1, result + doubling(k,1))
		| MONE(i) -> helper(i, k+1, result - doubling(k,1))
	in
	helper(x, 0, 0)

