(* not tested *)

type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro

and name = string

let rec lst_check (l: name list) (elt: name) = 
	match l with
	| hd::tl -> if (hd = elt) then true
				else (lst_check tl elt)
	| [] -> false

let checkMetro (m: metro) = 
	let rec checkMetro_sub (m: metro) (sl: name list) = 
		match m with
		| STATION n -> lst_check sl n
		| AREA (n, sub_metro) -> (checkMetro_sub sub_metro (sl@[n]))
		| CONNECT (m1, m2) -> (checkMetro_sub m1 sl) && (checkMetro_sub m2 sl)
	in
	checkMetro_sub m []
