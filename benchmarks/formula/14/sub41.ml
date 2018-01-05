type expr = NUM of int
          | PLUS of expr * expr
	  | MINUS of expr * expr

type formula = TRUE
             | FALSE
	     | NOT of formula
	     | ANDALSO of formula * formula
	     | ORELSE of formula * formula
	     | IMPLY of formula * formula
	     | LESS of expr * expr

let rec calculate e =
  match e with
  | NUM n -> n
  | PLUS (a, b) -> (calculate a) + (calculate b)
  | MINUS (a, b) -> (calculate a) - (calculate b)

let rec eval f =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT a -> not (eval a)
  | ANDALSO (a, b) -> (eval a) && (eval b)
  | ORELSE (a, b) -> (eval a) || (eval b)
  | IMPLY (a, b) -> ((eval a) = false) || (eval (ANDALSO (a, b)))
  | LESS (a, b) -> (calculate a) < (calculate b)
