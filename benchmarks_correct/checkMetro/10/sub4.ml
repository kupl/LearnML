(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-7 *)

type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
	and var = string

let rec checkPre (m, l) = match m with
		V a -> List.mem a l
		| P (a, b) -> checkPre (b, a :: l)
		| C (a, b) -> checkPre (a, l) && checkPre (b, l)

let rec check m = checkPre (m, [])

(*
(* test code *)
let _ = print_endline (string_of_bool (checkPre (P("a", V "a"), [])));
	print_endline (string_of_bool (checkPre (P("a", P("a", V "a")), [])));
	print_endline (string_of_bool (checkPre (P("a", P("b", C(V "a", V "b"))), [])));
	print_endline (string_of_bool (checkPre (P("a", C(V "a", P("b", V "a"))), [])));
	print_endline (string_of_bool (checkPre (P("a", V "b"), [])));
	print_endline (string_of_bool (checkPre (P("a", C(V "a", P("b", V "c"))), [])));
	print_endline (string_of_bool (checkPre (P("a", P("b", C(V "a", V "c"))), [])))
*)
