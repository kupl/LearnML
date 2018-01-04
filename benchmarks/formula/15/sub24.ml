type formula =
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr

 and expr = NUM of int
	  | PLUS of expr * expr
	  | MINUS of expr * expr

let rec eval_expr (e: expr): int =
  match e with
  | NUM(x) -> x
  | PLUS(x, y) -> eval_expr x + eval_expr y
  | MINUS(x, y) -> eval_expr x - eval_expr y

let rec eval (f: formula): bool =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT(a) -> if a = TRUE then false else true
  | ANDALSO(x, y) -> if x = TRUE && y = TRUE then true
		     else false
  | ORELSE(x, y) -> if x = FALSE && y = FALSE then false
		    else true
  | IMPLY(x, y) -> if x = FALSE && y = FALSE then true
		   else if x = FALSE && y = TRUE then true
		   else if x = TRUE && y = FALSE then false
		   else true
  | LESS(x, y) -> if(eval_expr x < eval_expr y) then true
		  else false
	  
