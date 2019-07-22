(*
	department : computer science & engineering
	student ID : 2012-11242 / name : Seon-bi, Park
*)

type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

let rec checking mtr arealst = 			(* metro -> list -> bool*)
	match mtr with
		| STATION(n) ->
		if (List.mem n arealst) = true then true
		else false
		| CONNECT(m,n) -> (checking m arealst) && (checking n arealst)
		| AREA(n,m) -> (checking m (n::arealst))

let checkMetro mtr = 					(* metro -> bool *)
	match mtr with
		| STATION(n) -> false
		| CONNECT(m,n) -> false
		| AREA(n,m) -> (checking mtr [])
