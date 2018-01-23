(* 4190.310 Programming Language			*
 * Homework #1 - Exercise 7 (CheckMetroMap)	*
 * 2008-11744 Jongwook Choi 				*)

type metro  = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
	and name = string

let rec checkMetro : metro -> bool = fun m ->
	let rec checkMetroAux env m =
		match m with
			  STATION name -> List.exists (fun t -> (t = name)) env
			| AREA (name, m') -> checkMetroAux (name :: env) m'
			| CONNECT (m1, m2) -> (checkMetroAux env m1) && (checkMetroAux env m2)
	in
		checkMetroAux [] m
;;

