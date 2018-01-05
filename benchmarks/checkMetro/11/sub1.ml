(* 2004-11951 Noh, Soon Hyun *)

type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

(* metrolist -> bool *)
(* true if correct, false if incorrect *)
let rec protoCheck areaList met  =
	match (areaList, met) with
	(* Will use pattern matching to get element from areaList *)
	(* Maybe I could've used internal list function...
	but this is more simple *)
	| ([], (STATION stationName)) -> false
	| ((a::remainList), (STATION stationName))
		-> (if a=stationName then true else false)
		|| (protoCheck remainList met)
	| (_, (AREA (areaName, met1))) ->
		(protoCheck (areaName::areaList) met1)
	| (_, (CONNECT (met1, met2))) ->
		((protoCheck areaList met1) && (protoCheck areaList met2))

let checkMetro met = (protoCheck [] met)

(* Test Code :: 
let print_bool a =
        if a=true then print_string "true\n"
        else print_string "false\n"

let _ = print_bool (checkMetro (AREA("a", STATION "a"))); print_char '\n'
let _ = print_bool (checkMetro (AREA("a", AREA("a", STATION "a")))); print_char '\n'
let _ = print_bool (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))); print_char '\n'
let _ = print_bool (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))); print_char '\n'

let _ = print_bool (checkMetro (AREA("a", STATION "b"))); print_char '\n'
let _ = print_bool (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))); print_char '\n'
let _ = print_bool (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))); print_char '\n'
*)
