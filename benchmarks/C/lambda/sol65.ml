type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check met =
	let rec checklst (lst, x) =
		match lst with
		[] -> false
		| h::t -> if (x = h) then true else checklst (t, x)
	in
	let rec f (m, lst) =
		match m with
		V x -> checklst (lst, x)
		| P (x, y) -> f (y, x::lst)
		| C (x, y) -> (f (x, lst)) && (f (y, lst))
	in
	f (met, [])

