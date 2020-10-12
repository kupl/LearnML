type lambda = V of var
           | P of var * lambda
		   | C of lambda * lambda
and var = string

let rec check m =
  let rec cm m l =
    match m with
	  V n -> if (List.mem n l) then true else false
	| P (n, m) -> (cm m (if (List.mem n l) then l else n::l))
	| C (m1, m2) -> ((cm m1 l) && (cm m2 l))
  in
    (cm m [])

let a = P("a", V "a")
let b = P("a", P("a", V "a"))
let c = P("a", P("b", C(V "a", V "b")))
let d = P("a", C(V "a", P("b", V "a")))
let e = P("a", V "b")
let f = P("a", C(V "a", P("b", V "c")))
let g = P("a", P("b", C(V "a", V "c")))
