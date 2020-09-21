type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  
let rec cal
= fun f -> match f  with
  Num x -> x
  |Plus(a, b) -> (cal a + cal b)
  |Minus(a, b) -> (cal a - cal b);;

let rec eval : formula -> bool
= fun f -> match f with
  True -> true
  |False -> false
  |Not(a) -> if eval(a) then false else true
  |AndAlso(b, c) -> (eval b) && (eval c)
  |OrElse(d, e)-> (eval d) || (eval e)
  |Imply(f, g) -> if (eval f = true && eval g = false) then false else true
  |Equal(h, k)->if cal(h) = cal(k) then true else false;;
  
eval (Imply(Imply(Not(True), AndAlso(False,OrElse(True, False))), True));;
eval (Equal(Num 1, Plus(Num 1, Num 2)));;
  
    