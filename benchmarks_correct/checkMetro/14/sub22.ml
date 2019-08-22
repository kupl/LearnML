type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

(*
let print_list f lst =
	let rec print_elements = function
	| [] -> ()
	| h::t -> f h; print_string ";"; print_elements t
	in
	print_string "[";
	print_elements lst;
	print_string "]";;
*)

let rec check met =
	let rec innerMetro m areavarlist =
		let search n1 n2 =
			(n1 = n2) 
		in
		match m with
		| V s -> (* print_list print_string areavarlist *) (List.exists (search s) areavarlist)
		| P (a, m1) -> (innerMetro m1 (a::areavarlist)) 
		| C (m1, m2) -> (innerMetro m1 areavarlist) && (innerMetro m2 areavarlist)
	in
	innerMetro met []

(*
let a81 = check (P("a", V "a")) 
let a82 = check (P("a", P("a", V "a"))) 
let a83 = check (P("a", P("b", C(V "a", V "b")))) 
let a84 = check (P("a", C(V "a", P("b", V "a")))) 

let a85 = check (P("a", V "b")) 
let a86 = check (P("a", C(V "a", P("b", V "c")))) 
let a87 = check (P("a", P("b", C(V "a", V "c")))) 

*)


