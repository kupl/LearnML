type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec check: lambda -> bool = fun x ->
	let rec listing m l = 
		(match m with
		| V st -> List.mem st l
		| P (avar, mm) -> listing mm (avar::l)
		| C (m1, m2) -> (listing m1 l) && (listing m2 l)) in
	listing x []

let x1 = P("a", V "a")
let x2 = P("a", P("a", V "a"))
let x3 = P("a", P("b", C(V "a", V "b")))
let x4 = P("a", C(V "a", P("b", V "a")))
let x5 = P("a", V "b")
let x6 = P("a", C(V "a", P("b", V "c")))
let x7 = P("a", P("b", C(V "a", V "c")))