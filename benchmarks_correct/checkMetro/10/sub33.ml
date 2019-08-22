type var = string
type lambda = V of var | P of var * lambda | C of lambda * lambda

let check metr =
	let rec listUnion a b = 
		match a with 
		h::t -> if (List.mem h b) then b
			else h::b
		|[] -> b
	in
	let rec subCheckMetro set met= 
		match met with
		V nam -> (List.mem nam set)
		|P (nam, me) -> (subCheckMetro (listUnion [nam] set) me)
		|C (me1, me2) -> (subCheckMetro set me1) && (subCheckMetro set me2)
	in
	subCheckMetro [] metr

