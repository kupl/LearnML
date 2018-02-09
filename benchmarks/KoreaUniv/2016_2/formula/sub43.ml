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



let rec eval formula a = 
  match formula with
     True -> true
    |False -> false
    |Not(p) -> not(eval p a)
    |AndAlso(p,q) -> (eval p a) && (eval q a)
    |OrElse(p,q) -> (eval p a) || (eval q a)
    |Imply(p,q) -> not(eval p a) || (eval q a)
    |Equal(p,q) -> true
