type lambda = V of var
		| P of var * lambda
		| C of lambda * lambda
and var = string

let check a = 
	let rec contains x lst = 
		match lst with
		| [] -> false
		| hd::rest -> if hd=x then true
				else contains x rest
	in
	let rec realcheck a lst =
		match a with
		| V x -> contains x lst
		| P (x, y) -> realcheck y (x::lst)
		| C (x, y) -> (realcheck x lst) && (realcheck y lst)
	in
	realcheck a []

	
