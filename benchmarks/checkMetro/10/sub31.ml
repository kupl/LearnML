type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
and name = string

let checkMetro metro =
	let rec is_member e l =
		match l with
		  [] -> false
		| h::t -> (e = h) || is_member e t
	in
	let rec check mtr alst =
		match mtr with
		  STATION (name) -> is_member name alst
		| AREA (name, m) -> check m (name::alst)
		| CONNECT (m1, m2) -> check m1 alst && check m2 alst
	in
	check metro []
