type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda
and var = string

let rec check_withList (m: lambda) (l: var list): bool =
	match m with
	| V var -> List.mem var l
	| P (var, lambda) -> check_withList lambda (var::l)
	| C (lambda1, lambda2) -> (check_withList lambda1 l) && (check_withList lambda2 l)

let rec check (m: lambda): bool =
	check_withList m []

(* using test *)
(*
let _ =
	let msg = string_of_bool (check (P("a", C(V "a", P("b", P("c", V "c")))))) in
	print_endline msg

let _ =
	let msg = string_of_bool (check (P("a", V "b"))) in
	print_endline msg

let _ =
	let msg = string_of_bool (check (P("a", P("a", V "a")))) in
	print_endline msg
*)