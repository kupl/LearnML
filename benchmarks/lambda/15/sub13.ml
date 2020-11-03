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
(*
let _= 
let print_bool x = print_endline (string_of_bool x) in 

let a81 = check (P("a", V "a")) in 
let a82 = check (P("a", P("a", V "a"))) in 
let a83 = check (P("a", P("b", C(V "a", V "b")))) in 
let a84 = check (P("a", C(V "a", P("b", V "a")))) in 
let a85 = check (P("a", V "b")) in 
let a86 = check (P("a", C(V "a", P("b", V "c")))) in 
let a87 = check (P("a", P("b", C(V "a", V "c")))) in 

print_bool(false = check ( V "a")); 
print_bool(true = check ( C (P ("a", V "a"), P ("b", P("a", C(V "b", V "a")))))); 
print_bool(false = check ( C (P ("c", V "c"), P ("b", P("a", C(V "b", V "c")))))); 
print_bool(true = a81); 
print_bool(true = a82); 
print_bool(true = a83); 
print_bool(true = a84); 
print_bool(false = a85); 
print_bool(false = a86); 
print_bool(false = a87) 
;;
*)