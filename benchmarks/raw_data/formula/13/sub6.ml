(* KIHWAN KANG HW01-2 *)

(* PREDEFINED TYPES *)
type formula = 
True
|False
|Not of formula
|AndAlso of formula * formula
|OrElse of formula * formula
|Imply of formula * formula
|Equal of exp * exp
and exp =
	Num of int
	|Plus of exp * exp
	|Minus of exp * exp
(* END OF PREDEFINED TYPES *)

let rec eval formula = 
	(* expEval EVALUATES exp TYPE *)
	let rec expEval exp =
		match exp with
		|Num a -> a
		|Plus (a, b) -> (expEval a) + (expEval b)
		|Minus (a, b) -> (expEval a) - (expEval b)
in
	match formula with
	|True -> true
	|False -> false
	|Not subformula -> 
		(
		match (eval subformula) with
		|true -> false
		|false -> true
		)
	|AndAlso (a, b) ->
		(
		match ((eval a), (eval b)) with
		|(true, true) -> true
		|_ -> false
		)
	|OrElse (a, b) ->
		(
		match ((eval a), (eval b)) with
		|(false, false) -> false
		|_ -> true
		)
	|Imply (a, b) ->
		(
		match ((eval a), (eval b)) with
		|(true, false) -> false
		|_ -> true
		)
	|Equal (a, b) -> 
		if (expEval a) = (expEval b) 
		then true 
		else false
