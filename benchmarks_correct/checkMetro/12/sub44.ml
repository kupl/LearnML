type lambda = V of var
			  | P of var * lambda
			  | C of lambda * lambda
and var = string

let check m =
	let rec check_ m (al, nl) =
		match m with
			| V n -> (List.for_all (fun x -> List.mem x al) (n::nl))
			| P (n, m_) -> check_ m_ (n::al, nl)
			| C (m_1, m_2) -> (check_ m_1 (al, nl)) && (check_ m_2 (al, nl)) in
	check_ m ([], [])
	
(* TEST SET *)
(*
check (V "a");;
check (P("a", V "a"));;
check (P("a", P("b", P ("c", C(V "c", V "c")))));;
check (P("b", C(V "a", V "b")));;
check (P("a", C(V "a", P("b", P ("b", V "b")))));;
check (P("a", P("a", V "a")));;
check (P("a", P("b", C(V "a", V "b"))));;
check (P("a", C(V "a", P("b", V "a"))));;
check (P("a", V "b"));;
check (P("a", C(V "a", P("b", V "c"))));;
check (P("a", P("b", C(V "a", V "c"))));;
*)