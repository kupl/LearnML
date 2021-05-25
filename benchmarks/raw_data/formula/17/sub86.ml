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
    
    let rec exp2int : exp -> int = function
      | Num(a) -> a
      | Plus(a, b) -> exp2int(a) + exp2int(b)
      | Minus(a, b) -> exp2int(a) - exp2int(b)
    
    
    let rec eval : formula -> bool = function
      | True -> true
      | False -> false
      | Not(a) -> not(eval(a))
      | AndAlso(a, b) -> eval(a) && eval(b)
      | OrElse(a,b) -> eval(a) || eval(b)
      | Imply(a,b) -> not(eval(a)) || eval(b)
      | Equal(a, b) -> exp2int(a) = exp2int(b)
        