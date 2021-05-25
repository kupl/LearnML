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

let rec eval : formula -> bool
= fun f -> match f with
  | True -> true
  | False -> false
  | Not(p) -> if p = True then false else true
  | AndAlso(p, q) -> if p = False then false else if q = False then false else true
  | OrElse(p, q) -> if p = True then true else if q = True then true else false
  | Imply(p, q) -> if q = True then true else if p = False then true else false
  | Equal(n1, n2) -> let rec exp_to_int : exp -> int 
                     = fun e -> match e with
                      | Num n -> n
                      | Plus(n1, n2) -> exp_to_int n1 + exp_to_int n2
                      | Minus(n1, n2) -> exp_to_int n1 - exp_to_int n2
                        in if exp_to_int n1 = exp_to_int n2 then true else false;;
  
eval (Imply (Imply (True, False), True));;
eval (Equal(Num 1, Plus(Num 1, Num 2)));;
