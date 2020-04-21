type formula  = True 
| False 
| Not of formula 
| AndAlso of formula * formula 
| OrElse of formula * formula 
| Imply of formula * formula 
| Equal of exp * exp
and exp = Num of int 
| Plus of exp * exp 
| Minus of exp * exp

let rec eval fm =
  let imp a b =
    if a = true && b = false
    then false 
    else true
  in
  let rec exp ex =
    match ex with
    |Num a -> a
    |Plus (a, b) -> (exp a) + (exp b)
    |Minus (a, b) -> (exp a) - (exp b)
  in
  match fm with
  |True -> true
  |False -> false
  |Not a -> not (eval a)
  |AndAlso (a, b) -> (eval a) && (eval b)
  |OrElse (a, b) -> (eval a) || (eval b)
  |Imply (a, b) -> (imp (eval a) (eval b))
  |Equal (a, b) -> (exp a) = (exp b)
