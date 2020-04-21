(* hw 1_5. *)
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
let rec eval f =
	let rec calcu e =
		match e with
		 Num n -> n
		|Plus(n1, n2) -> calcu n1 + calcu n2
		|Minus(n1, n2) -> calcu n1 - calcu n2 in
	match f with
	 True -> true
	|False -> false
	|Not a ->
		(if eval a = true then false
		 else true)
	|AndAlso(a, b) ->
		(match (eval a, eval b) with
		 (true, true) -> true
		 |_ -> false)
	|OrElse(a, b) ->
		(match (eval a, eval b) with
		 (false, false) -> false
		 |_ -> true)
	|Imply(a, b) ->
		(match (eval a, eval b) with
		 (true, false) -> false
		|_ -> true)
	|Equal(a, b) ->
		(if calcu a = calcu b then true
		 else false)
