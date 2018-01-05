(* HW1 exercise8 2009-11697 Kim HyunJoon *)
(* CheckMetroMap *)

type metro 	= STATION of name
		| AREA of name * metro
		| CONNECT of metro * metro

and name = string

let checkMetro : metro -> bool =
	fun met ->
	let rec myCheckMetro m lst =	
		match m with
		| STATION s -> (List.mem s lst)
		| AREA (a, n) -> (myCheckMetro n (a::lst))
		| CONNECT (m1, m2) -> (myCheckMetro m1 lst) && (myCheckMetro m2 lst)
	in
	(myCheckMetro met [])

