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

let rec eval p =
  match p with 
    True  -> true
  | False -> false
  | Not a -> not (eval a)
  | AndAlso (a, b) -> (eval a) && (eval b)
  | OrElse  (a, b) -> (eval a) || (eval b) 
  | Imply   (a, b) -> (eval (Not a)) || (eval b)
  | Equal    (a, b) -> if (my_exp a) = (my_exp b) then true else false
and my_exp p =
  match p with
    Num b -> b
  | Plus  (a, b) -> (my_exp a) + (my_exp b)
  | Minus (a, b) -> (my_exp a) - (my_exp b)
