(* 2004-11951 Noh, Soon Hyun *)

type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

(* lambdalist -> bool *)
(* true if correct, false if incorrect *)
let rec protoCheck areaList met  =
	match (areaList, met) with
	(* Will use pattern matching to get element from areaList *)
	(* Maybe I could've used internal list function...
	but this is more simple *)
	| ([], (V stationName)) -> false
	| ((a::remainList), (V stationName))
		-> (if a=stationName then true else false)
		|| (protoCheck remainList met)
	| (_, (P (areaName, met1))) ->
		(protoCheck (areaName::areaList) met1)
	| (_, (C (met1, met2))) ->
		((protoCheck areaList met1) && (protoCheck areaList met2))

let check met = (protoCheck [] met)

(* Test Code :: 
let print_bool a =
        if a=true then print_string "true\n"
        else print_string "false\n"

let _ = print_bool (check (P("a", V "a"))); print_char '\n'
let _ = print_bool (check (P("a", P("a", V "a")))); print_char '\n'
let _ = print_bool (check (P("a", P("b", C(V "a", V "b"))))); print_char '\n'
let _ = print_bool (check (P("a", C(V "a", P("b", V "a"))))); print_char '\n'

let _ = print_bool (check (P("a", V "b"))); print_char '\n'
let _ = print_bool (check (P("a", C(V "a", P("b", V "c"))))); print_char '\n'
let _ = print_bool (check (P("a", P("b", C(V "a", V "c"))))); print_char '\n'
*)
