(* complete *)
exception Invalid_Input

let sigma f a b =
	let rec iter(n) =
		if n = b then f(n)
			else f(n) + iter(n+1)
		in
	if a > b then raise Invalid_Input
		else iter(a)
;;
