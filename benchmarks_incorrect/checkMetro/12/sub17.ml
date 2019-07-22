(* 2008-11874 EXERCISE 8 *)

type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro metro =
	let rec nameStation metro =
		match metro with
			| STATION(n) -> n::[]
			| AREA(n,m) -> nameStation m
			| CONNECT(m1,m2) -> List.append (nameStation m1) (nameStation m2)
	in
	
	let rec nameArea metro =
		match metro with
			| STATION(n) -> []
			| AREA(n,m) -> n::(nameArea m)
			| CONNECT(m1,m2) -> List.append (nameArea m1) (nameArea m2)
	in
	
	let rec check l1 l2 =
		match l1 with
			| [] -> true
			| hd::tl -> (List.mem hd l2) && (check tl l2)
	in
	
	check (nameStation metro) (nameArea metro)
	
	