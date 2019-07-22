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

let rec eval : formula->bool = fun fm ->
   let rec eval_expr : expr->int = fun e ->
	match e with
	NUM n->n
	|PLUS(e1,e2)->eval_expr e1 + eval_expr e2
	|MINUS(e1,e2)->eval_expr e1 - eval_expr e2
	in

   match fm with
     TRUE->true
   | FALSE->false
   | NOT fm -> if eval fm then false else true
   | ANDALSO(f1, f2)-> if eval f1&&eval f2 then true else false
   | ORELSE(f1, f2)-> if eval f1||eval f2 then true else false
   | IMPLY(f1, f2)-> 
   if eval f1==false||eval f2==true then true
   else false
   | LESS(e1, e2)-> if eval_expr e1 < eval_expr e2 then true else false