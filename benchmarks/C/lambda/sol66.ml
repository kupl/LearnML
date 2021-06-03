(* 2009-13384, CHO Hyunik *)



type lambda =	V of var
		| P of var * lambda
		| C of lambda * lambda
and var = string



let rec check met =

(* LOCAL FUNCTION
	correctMetro : check that met is correct or not correct. list includes every element which 'covers' met.
	ex. correctMetro [] P("a", P("b", P("c", P("aaa", V("bbb")))))
		= ...
		= correctMetro ["a"; "b"; "c"] P("aaa", V("bbb"))
		= correctMetro ["aaa"; "a"; "b"; "c"] V("bbb")
		= false
	*) 
	let rec correctMetro lst met =
		match met with
		P(a, b) -> correctMetro (a::lst) b
		| C(a, b) -> (correctMetro lst a) && (correctMetro lst b)
		| V a -> List.mem a lst in


	correctMetro [] met
