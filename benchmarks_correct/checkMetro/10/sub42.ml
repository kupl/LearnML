type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check m =
let rec check m pl = match m with
	  V n -> (List.mem n pl)
	| P (n,m1) -> (check m1 (n::pl))
	| C (m1,m2) -> (check m1 pl) && (check m2 pl)
in
	(check m [])

(**
P("a", V "a")
P("a", P("a", V "a"))
P("a", P("b", C(V "a", V "b")))
P("a", C(V "a", P("b", V "a")))

P("a", V "b")
P("a", C(V "a", P("b", V "c")))
P("a", P("b", C(V "a", V "c")))
*)

