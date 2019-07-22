type name = string
type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro

let checkMetro metr =
	let rec listUnion a b = 
		match a with 
		h::t -> if (List.mem h b) then b
			else h::b
		|[] -> b
	in
	let rec subCheckMetro set met= 
		match met with
		STATION nam -> (List.mem nam set)
		|AREA (nam, me) -> (subCheckMetro (listUnion [nam] set) me)
		|CONNECT (me1, me2) -> (subCheckMetro set me1) && (subCheckMetro set me2)
	in
	subCheckMetro [] metr

