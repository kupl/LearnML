(* HW1 exercise8 2009-11697 Kim HyunJoon *)
(* CheckMetroMap *)

type lambda 	= V of var
		| P of var * lambda
		| C of lambda * lambda

and var = string

let check : lambda -> bool =
	fun met ->
	let rec myCheckMetro m lst =	
		match m with
		| V s -> (List.mem s lst)
		| P (a, n) -> (myCheckMetro n (a::lst))
		| C (m1, m2) -> (myCheckMetro m1 lst) && (myCheckMetro m2 lst)
	in
	(myCheckMetro met [])

