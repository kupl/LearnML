type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string

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

let rec checkMetro met =
	let rec innerMetro m areanamelist =
		let search n1 n2 =
			(n1 = n2) 
		in
		match m with
		| STATION s -> (* print_list print_string areanamelist *) (List.exists (search s) areanamelist)
		| AREA (a, m1) -> (innerMetro m1 (a::areanamelist)) 
		| CONNECT (m1, m2) -> (innerMetro m1 areanamelist) && (innerMetro m2 areanamelist)
	in
	innerMetro met []

(*
let a81 = checkMetro (AREA("a", STATION "a")) 
let a82 = checkMetro (AREA("a", AREA("a", STATION "a"))) 
let a83 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) 
let a84 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) 

let a85 = checkMetro (AREA("a", STATION "b")) 
let a86 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) 
let a87 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) 

*)


