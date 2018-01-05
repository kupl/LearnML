type formula = TRUE | FALSE
	|NOT of formula
	|ANDALSO of formula * formula
	|ORELSE of formula * formula
	|IMPLY of formula * formula
	|LESS of expr * expr
and expr = NUM of int
	|PLUS of expr * expr
	|MINUS of expr * expr

let rec eval x =
	
	(* Calculate value *)
	let rec calexpr t =
		match t with
		NUM a-> a
		|PLUS (a, b)->(calexpr a) + (calexpr b)
		|MINUS (a, b)->(calexpr a) - (calexpr b)
	in


	match x with
	|TRUE -> true
	|FALSE -> false
	|NOT a -> not (eval a)
	|ANDALSO (a, b) -> (eval a)&&(eval b)
	|ORELSE (a, b) -> (eval a)||(eval b)
	|IMPLY (a, b) -> (not (eval a))||(eval b)
	|LESS (a, b) -> if (calexpr a)<(calexpr b) then true else false
