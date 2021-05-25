(*
** PL::HW[01].Problem[02]
** 
** Last Mod.: 2014-09-14 20:34
** Writ. by : CMS
*)

type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp

    and exp = Num of int
             | Plus of exp * exp
             | Minus of exp * exp

let rec calc e = 
	match e with
	| Num i -> i
	| Plus (le, re) -> calc le + calc re
	| Minus (le, re) -> calc le - calc re

let rec eval e = 
	match e with
	| True -> true
	| False -> false
	| Not f -> if eval f = true then false else true
	| AndAlso (l, r) -> 
		if eval l = true
		then
			if eval r = true
			then true
			else false
		else false
	| OrElse (l, r) -> 
		if eval l = false
		then
			if eval r = false
			then false
			else true
		else true
	| Imply (l, r) -> 
		if eval l = false
		then true
		else
			if eval r = true
			then true
			else false
	| Equal (le, re) ->
		if calc le = calc re
		then true
		else false

