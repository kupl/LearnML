
(* ex 7 *)

type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro 

and name = string

let checkMetro met =
	let rec checkList(s,l) =
		match l with
		| [] -> false
		| h::t -> if s=h then true else checkList(s,t)
	in	

	let rec cal(m,l) =
		match m with 
		| STATION n -> checkList(n,l) 
		| AREA(n,m1) -> cal(m1,n::l) 
		| CONNECT(m1,m2) -> cal(m1,l) && cal(m2,l)
	in

	cal(met,[])


