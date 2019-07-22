(* KIHWAN KANG HW01-2 *)

(* PREDEFINED TYPES *)
type formula = 
TRUE
|FALSE
|NOT of formula
|ANDALSO of formula * formula
|ORELSE of formula * formula
|IMPLY of formula * formula
|LESS of expr * expr
and expr =
	NUM of int
	|PLUS of expr * expr
	|MINUS of expr * expr
(* END OF PREDEFINED TYPES *)

let rec eval formula = 
	(* exprEval EVALUATES expr TYPE *)
	let rec exprEval expr =
		match expr with
		|NUM a -> a
		|PLUS (a, b) -> (exprEval a) + (exprEval b)
		|MINUS (a, b) -> (exprEval a) - (exprEval b)
in
	match formula with
	|TRUE -> true
	|FALSE -> false
	|NOT subformula -> 
		(
		match (eval subformula) with
		|true -> false
		|false -> true
		)
	|ANDALSO (a, b) ->
		(
		match ((eval a), (eval b)) with
		|(true, true) -> true
		|_ -> false
		)
	|ORELSE (a, b) ->
		(
		match ((eval a), (eval b)) with
		|(false, false) -> false
		|_ -> true
		)
	|IMPLY (a, b) ->
		(
		match ((eval a), (eval b)) with
		|(true, false) -> false
		|_ -> true
		)
	|LESS (a, b) -> 
		if (exprEval a) < (exprEval b) 
		then true 
		else false
