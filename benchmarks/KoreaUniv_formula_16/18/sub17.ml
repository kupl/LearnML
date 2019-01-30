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

let rec eval  formula =
  match formula with
  |True -> true
  |False -> false
  |Not(a) -> not (eval a)
  |AndAlso(a, b) -> eval a && eval b
  |OrElse(a, b) ->  eval a || eval b
  |Imply(a, b) -> eval a <= eval b
  |Equal(a, b) -> if exp a == exp b then eval True else eval False
and exp e =
  match e with
  |Num i -> i  
  |Plus(a, b) -> exp a + exp b
  |Minus(a, b) -> exp a - exp b
  ;;
  eval (Imply(Imply(True, False), True));;
  eval (Equal(Num 1, Plus(Num 1, Num 2)));;
  
  
  