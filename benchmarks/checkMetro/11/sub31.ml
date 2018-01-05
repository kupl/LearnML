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
	let rec correctMetro list met =
		match met with
		AREA(a, b) -> correctMetro (a::list) b
		| CONNECT(a, b) -> (correctMetro list a) && (correctMetro list b)
		| STATION a -> List.mem a list in


	correctMetro [] met