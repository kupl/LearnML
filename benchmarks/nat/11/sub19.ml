(* 2009-11674 ±è¿øÁø HW1-6 *)

type nat = ZERO | SUCC of nat

let rec trans(a, res)=
	match a with
		| ZERO -> res
		| SUCC x -> trans(x, (res +1)) 

let rec makeNat(n, res) =
	if (n = 0) then res
	else makeNat((n-1), (SUCC res))

let natadd(a, b) =
	(makeNat((trans(a, 0) + trans(b, 0)), ZERO))
		
let natmul(a, b) =
	(makeNat((trans(a, 0) * trans(b, 0)), ZERO))
