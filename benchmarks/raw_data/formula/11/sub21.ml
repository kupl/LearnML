(* 컴퓨터공학부/2009-11679/김정명/5 *)

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

let rec eval form =
	let rec calc e = 
		match e with
		  Num n -> n
		| Plus (e1, e2) -> calc e1 + calc e2
		| Minus (e1, e2) -> calc e1 - calc e2
	in

	match form with 
	  True -> true
	| False -> false
	| Not f -> not (eval f)
	| AndAlso (f1, f2) -> 
		if eval f1 && eval f2 then true
		else false
	| OrElse (f1, f2) ->
		if eval f1 || eval f2 then true
		else false
	| Imply (f1, f2) ->
		if eval f1 && not (eval f2) then false
		else true
	| Equal (e1, e2) ->
		if calc e1 = calc e2 then true
		else false
