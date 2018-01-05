(* hw 1_5. *)
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
let rec eval f =
	let rec calcu e =
		match e with
		 NUM n -> n
		|PLUS(n1, n2) -> calcu n1 + calcu n2
		|MINUS(n1, n2) -> calcu n1 - calcu n2 in
	match f with
	 TRUE -> true
	|FALSE -> false
	|NOT a ->
		(if eval a = true then false
		 else true)
	|ANDALSO(a, b) ->
		(match (eval a, eval b) with
		 (true, true) -> true
		 |_ -> false)
	|ORELSE(a, b) ->
		(match (eval a, eval b) with
		 (false, false) -> false
		 |_ -> true)
	|IMPLY(a, b) ->
		(match (eval a, eval b) with
		 (true, false) -> false
		|_ -> true)
	|LESS(a, b) ->
		(if calcu a < calcu b then true
		 else false)
