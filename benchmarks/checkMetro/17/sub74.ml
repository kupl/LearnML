
(* EXERCISE 4 *) 

type name = string

type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro

let rec find_namelist : name -> (name list) -> bool = fun input l ->
	match l with
	| [] -> false
	| h :: t -> if h = input then true
		else (find_namelist input t)

let rec checkMetroProbe : metro * (name list) -> bool = fun (input , nlist) ->
	match input with
	| STATION (station_name) -> (find_namelist station_name nlist)
	| AREA (area_name, subinput) -> checkMetroProbe (subinput , (area_name :: nlist))
	| CONNECT (subinput1, subinput2) -> checkMetroProbe(subinput1, nlist) && checkMetroProbe(subinput2, nlist)


let checkMetro : metro -> bool = fun input -> checkMetroProbe (input, [])

(*
let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool(true = (find_namelist "a" ("b"::"a"::"c"::[])))
*)	
(*
let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool(true = checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a"))))));
print_bool(false = checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c")))))); 
print_bool(true = checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))));
print_bool(true = checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))));
print_bool(true = checkMetro(AREA("a", AREA("a", STATION "a"))));
print_bool(true = checkMetro(AREA("a", STATION "a")));
print_bool(false = checkMetro(AREA("a", STATION "b")));
print_bool(false = checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))));
print_bool(false = checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))));
*)
(*
let _ = 
let test_case : int * bool -> unit = fun (n, x) -> 
print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
test_case(1, true == checkMetro(AREA("a", STATION "a"))); 
test_case(2, true == checkMetro(AREA("a", AREA("a", STATION "a")))); 
test_case(3, true == checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))); 
test_case(4, true == checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))); 
test_case(5, false == checkMetro(AREA("a", STATION "b"))); 
test_case(6, false == checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))); 
test_case(7, false == checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))); 
test_case(8, true == checkMetro(CONNECT(AREA("a", STATION "a"), AREA("b", AREA("a", CONNECT(STATION "b", STATION "a")))))); 
test_case(9, false == checkMetro(CONNECT(AREA("c", STATION "c"), AREA("b", AREA("a", CONNECT(STATION "b", STATION "c")))))); 
test_case(10, false == checkMetro(STATION "a"));
*)
