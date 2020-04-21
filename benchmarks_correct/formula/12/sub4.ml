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

let rec eval : formula->bool = fun fm ->
   let rec eval_exp : exp->int = fun e ->
	match e with
	Num n->n
	|Plus(e1,e2)->eval_exp e1 + eval_exp e2
	|Minus(e1,e2)->eval_exp e1 - eval_exp e2
	in

   match fm with
     True->true
   | False->false
   | Not fm -> if eval fm then false else true
   | AndAlso(f1, f2)-> if eval f1&&eval f2 then true else false
   | OrElse(f1, f2)-> if eval f1||eval f2 then true else false
   | Imply(f1, f2)-> 
   if eval f1==false||eval f2==true then true
   else false
   | Equal(e1, e2)-> if eval_exp e1 = eval_exp e2 then true else false