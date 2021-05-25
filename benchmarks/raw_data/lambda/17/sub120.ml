(*2016-11690*)
type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check : lambda -> bool = fun metr ->
	let rec check : lambda ->'a list -> bool = fun inputMetro areaList ->
		match inputMetro with
		| V var -> List.mem var areaList
		| P (var,met) -> check met (var::areaList)
		| C (met1,met2) -> if (check met1 areaList) then (check met2 areaList) else false
	in
	check metr []

(*test code 
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