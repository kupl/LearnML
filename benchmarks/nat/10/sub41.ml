(* complete *)
type nat = ZERO | SUCC of nat

let rec count n = match n with
	ZERO -> 0
	|SUCC a -> 1 + count a
let rec generate n =
	if n = 0 then ZERO
		else (SUCC (generate (n-1)))

let natadd (a,b) =
	generate ((count a) + (count b))
let natmul (a,b) =
	generate ((count a) * (count b))
;;
