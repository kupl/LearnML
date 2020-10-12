
(* EXERCISE 4 *) 

type var = string

type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda

let rec find_varlist : var -> (var list) -> bool = fun input l ->
	match l with
	| [] -> false
	| h :: t -> if h = input then true
		else (find_varlist input t)

let rec checkProbe : lambda * (var list) -> bool = fun (input , nlist) ->
	match input with
	| V (station_var) -> (find_varlist station_var nlist)
	| P (area_var, subinput) -> checkProbe (subinput , (area_var :: nlist))
	| C (subinput1, subinput2) -> checkProbe(subinput1, nlist) && checkProbe(subinput2, nlist)


let check : lambda -> bool = fun input -> checkProbe (input, [])

(*
let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool(true = (find_varlist "a" ("b"::"a"::"c"::[])))
*)	
(*
let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool(true = check ( C (P ("a", V "a"), P ("b", P("a", C(V "b", V "a"))))));
print_bool(false = check ( C (P ("c", V "c"), P ("b", P("a", C(V "b", V "c")))))); 
print_bool(true = check(P("a", C(V "a", P("b", V "a")))));
print_bool(true = check(P("a", P("b", C(V "a", V "b")))));
print_bool(true = check(P("a", P("a", V "a"))));
print_bool(true = check(P("a", V "a")));
print_bool(false = check(P("a", V "b")));
print_bool(false = check(P("a", C(V "a", P("b", V "c")))));
print_bool(false = check(P("a", P("b", C(V "a", V "c")))));
*)
(*
let _ = 
let test_case : int * bool -> unit = fun (n, x) -> 
print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
test_case(1, true == check(P("a", V "a"))); 
test_case(2, true == check(P("a", P("a", V "a")))); 
test_case(3, true == check(P("a", P("b", C(V "a", V "b"))))); 
test_case(4, true == check(P("a", C(V "a", P("b", V "a"))))); 
test_case(5, false == check(P("a", V "b"))); 
test_case(6, false == check(P("a", C(V "a", P("b", V "c"))))); 
test_case(7, false == check(P("a", P("b", C(V "a", V "c"))))); 
test_case(8, true == check(C(P("a", V "a"), P("b", P("a", C(V "b", V "a")))))); 
test_case(9, false == check(C(P("c", V "c"), P("b", P("a", C(V "b", V "c")))))); 
test_case(10, false == check(V "a"));
*)
