type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
and name = string

let rec checkMetro_withList (m: metro) (l: name list): bool =
	match m with
	| STATION name -> List.mem name l
	| AREA (name, metro) -> checkMetro_withList metro (name::l)
	| CONNECT (metro1, metro2) -> (checkMetro_withList metro1 l) && (checkMetro_withList metro2 l)

let rec checkMetro (m: metro): bool =
	checkMetro_withList m []

(* using test *)
(*
let _ =
	let msg = string_of_bool (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", AREA("c", STATION "c")))))) in
	print_endline msg

let _ =
	let msg = string_of_bool (checkMetro (AREA("a", STATION "b"))) in
	print_endline msg

let _ =
	let msg = string_of_bool (checkMetro (AREA("a", AREA("a", STATION "a")))) in
	print_endline msg
*)
(*
let _= 
let print_bool x = print_endline (string_of_bool x) in 

let a81 = checkMetro (AREA("a", STATION "a")) in 
let a82 = checkMetro (AREA("a", AREA("a", STATION "a"))) in 
let a83 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) in 
let a84 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) in 
let a85 = checkMetro (AREA("a", STATION "b")) in 
let a86 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) in 
let a87 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) in 

print_bool(false = checkMetro ( STATION "a")); 
print_bool(true = checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a")))))); 
print_bool(false = checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c")))))); 
print_bool(true = a81); 
print_bool(true = a82); 
print_bool(true = a83); 
print_bool(true = a84); 
print_bool(false = a85); 
print_bool(false = a86); 
print_bool(false = a87) 
;;
*)