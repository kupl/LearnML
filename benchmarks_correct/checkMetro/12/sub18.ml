(* ex8 *)
type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

let checkMetro m = 
	let rec checker (met, lst) = 
		match met with
		  AREA (str, met_) -> checker (met_, str::lst)
		| CONNECT (met1, met2) -> (checker (met1, lst)) && (checker (met2, lst))
		| STATION str -> List.mem str lst
	in
	checker (m, [])