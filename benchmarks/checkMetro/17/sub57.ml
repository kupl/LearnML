(*Lee Seok Jin 2013-11417 CSE hw2_4*)

type metro	= STATION of name
		| AREA of name * metro
		| CONNECT of metro * metro

and name = string

(* using anonymous function would be better as we learned in PP
let compare((st, stan): 'a * 'a): bool = 
*)

let checkMetro(m: metro) : bool = 
	let rec rec_checkMetro((met:metro),(area_list:string list)): bool = 
		match met with 
		| STATION(s) -> List.exists(fun x -> x=s) area_list
		| AREA(s, _m) -> rec_checkMetro(_m, s::area_list)
		| CONNECT(l,r) -> rec_checkMetro(l, area_list) && rec_checkMetro(r,area_list)
	in rec_checkMetro(m, [])
