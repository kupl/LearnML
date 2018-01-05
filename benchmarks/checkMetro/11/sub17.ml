(* 2009-11674 ±è¿øÁø HW1-7 *)

type metro = STATION of name
	   | AREA of name * metro
	   | CONNECT of metro * metro
and name = string

let checkMetro m =

	let rec test(a, lst) =
        	match lst with
                	| x::l -> (if (a=x) then true
                           	   else test(a, l))
                	| [] -> false
	in

	let rec check(m, lst)=
	        match m with
        	        | STATION a -> test(a, lst)
	               	| AREA(a, met) -> check(met, a::lst)
	                | CONNECT(m1, m2) -> (check(m1,lst) & check(m2,lst))
	in

	check(m, [])
