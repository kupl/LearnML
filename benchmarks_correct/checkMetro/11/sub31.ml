(* 2009-13384, CHO Hyunik *)



type metro =	STATION of name
		| AREA of name * metro
		| CONNECT of metro * metro
and name = string



let rec checkMetro met =

(* LOCAL FUNCTION
	correctMetro : check that met is correct or not correct. list includes every element which 'covers' met.
	ex. correctMetro [] AREA("a", AREA("b", AREA("c", AREA("aaa", STATION("bbb")))))
		= ...
		= correctMetro ["a"; "b"; "c"] AREA("aaa", STATION("bbb"))
		= correctMetro ["aaa"; "a"; "b"; "c"] STATION("bbb")
		= false
	*) 
	let rec correctMetro lst met =
		match met with
		AREA(a, b) -> correctMetro (a::lst) b
		| CONNECT(a, b) -> (correctMetro lst a) && (correctMetro lst b)
		| STATION a -> List.mem a lst in


	correctMetro [] met
