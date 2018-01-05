(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-7 *)

type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
	and name = string

let rec checkMetroPre (m, l) = match m with
		STATION a -> List.mem a l
		| AREA (a, b) -> checkMetroPre (b, a :: l)
		| CONNECT (a, b) -> checkMetroPre (a, l) && checkMetroPre (b, l)

let rec checkMetro m = checkMetroPre (m, [])

(*
(* test code *)
let _ = print_endline (string_of_bool (checkMetroPre (AREA("a", STATION "a"), [])));
	print_endline (string_of_bool (checkMetroPre (AREA("a", AREA("a", STATION "a")), [])));
	print_endline (string_of_bool (checkMetroPre (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))), [])));
	print_endline (string_of_bool (checkMetroPre (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))), [])));
	print_endline (string_of_bool (checkMetroPre (AREA("a", STATION "b"), [])));
	print_endline (string_of_bool (checkMetroPre (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))), [])));
	print_endline (string_of_bool (checkMetroPre (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))), [])))
*)
