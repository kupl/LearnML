type formula = True
  |False
  |Not of formula
  |AndAlso of formula*formula
  |OrElse of formula*formula
  |Imply of formula*formula
  |Equal of exp*exp
  and exp = Num of int
  |Plus of exp*exp
  |Minus of exp*exp

let rec exp_to_int : exp -> int = fun e ->
match e with
|Num a -> a
|Plus (e1,e2) -> (exp_to_int e1)+(exp_to_int e2)
|Minus (e1,e2) -> (exp_to_int e1)-(exp_to_int e2) 

let less : exp*exp -> bool = fun (e1,e2) -> (exp_to_int e1)=(exp_to_int e2) 

let rec eval : formula -> bool = fun f ->
  match f with
  |True -> true
  |False -> false
  |Not f1-> not (eval f1)
  |AndAlso(f1,f2) -> (eval f1)&&(eval f2)
  |OrElse(f1,f2) -> (eval f1)||(eval f2)
  |Imply (f1,f2) -> (not (eval f1))||(eval f2)
  |Equal (e1,e2) -> less (e1,e2)



