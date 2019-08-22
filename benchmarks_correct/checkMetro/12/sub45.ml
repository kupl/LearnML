type lambda = V of var | P of var * lambda | C of lambda * lambda
	and var = string

let check mat =
	let rec chk mat stl =
		match mat with
		| V a -> (List.mem a stl)
		| P (a, b) -> (chk b (a::stl))
		| C (a, b) -> (chk a stl) && (chk b stl)
	in
	chk mat []


