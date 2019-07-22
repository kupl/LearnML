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