(*
** PL::HW[01].Problem[02]
** 
** Last Mod.: 2014-09-14 20:34
** Writ. by : CMS
*)

type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr

    and expr = NUM of int
             | PLUS of expr * expr
             | MINUS of expr * expr

let rec calc e = 
	match e with
	| NUM i -> i
	| PLUS (le, re) -> calc le + calc re
	| MINUS (le, re) -> calc le - calc re

let rec eval e = 
	match e with
	| TRUE -> true
	| FALSE -> false
	| NOT f -> if eval f = true then false else true
	| ANDALSO (l, r) -> 
		if eval l = true
		then
			if eval r = true
			then true
			else false
		else false
	| ORELSE (l, r) -> 
		if eval l = false
		then
			if eval r = false
			then false
			else true
		else true
	| IMPLY (l, r) -> 
		if eval l = false
		then true
		else
			if eval r = true
			then true
			else false
	| LESS (le, re) ->
		if calc le < calc re
		then true
		else false

