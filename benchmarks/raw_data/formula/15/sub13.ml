(* Ex 4. True or False *)
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

let rec calc_exp e =
	match e with
	| Num e -> e
	| Plus (e1, e2) -> (calc_exp e1) + (calc_exp e2)
	| Minus (e1, e2) -> (calc_exp e1) - (calc_exp e2)

let rec eval f =
	match f with
	| True -> true
	| False -> false
	| Not sf -> not (eval sf)
	| AndAlso (sf1, sf2) -> (eval sf1) && (eval sf2)
	| OrElse (sf1, sf2) -> (eval sf1) || (eval sf2)
	| Imply (sf1, sf2) ->
		(match (eval sf1) with
		| true -> eval sf2
		| false -> true)
	| Equal (e1, e2) ->
		(if (calc_exp e1) = (calc_exp e2) then true
		else false)
(*
let _ =
	let msg = string_of_bool (eval True) in
	print_endline msg

let _ =
	let msg = string_of_bool (eval (AndAlso(True, False))) in
	print_endline msg

let _ =
	let msg = string_of_bool (eval (Equal(Num 3, Plus(Num 2, Num 4)))) in
	print_endline msg
*)