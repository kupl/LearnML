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
  let rec evalExpr e = match e with
      Num i -> i
	| Plus (e1, e2) -> (evalExpr e1) + (evalExpr e2)
	| Minus (e1, e2) -> (evalExpr e1) - (evalExpr e2)
  in
    match f with
	  True -> true
	| False -> false
	| Not f -> not (eval f)
	| AndAlso (f1, f2) -> if (eval f1) then (eval f2) else false
	| OrElse (f1, f2) -> if (eval f1) then true else (eval f2)
	| Imply (f1, f2) -> if (eval f1) then (eval f2) else true
	| Equal (e1, e2) -> (evalExpr e1) = (evalExpr e2)
