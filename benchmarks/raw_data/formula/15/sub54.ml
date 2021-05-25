type formula = True
               | False
               | Not of formula
               | AndAlso of formula * formula
               | OrElse of formula * formula
               | Imply of formula * formula
               | Equal of exp * exp
and exp = Num of int
           | Plus of exp * exp
           | Minus of exp * exp;;

let rec eval_exp = function
  | Num a -> a
  | Plus (a, b) -> eval_exp a + eval_exp b
  | Minus (a, b) -> eval_exp a - eval_exp b;;

let rec eval = function
  | True -> true
  | False -> false
  | Not a -> if (eval a) = true then false else true
  | AndAlso (a, b) -> if (eval a) = true && (eval b) = true then true else false
  | OrElse (a, b) -> if (eval a) = true || (eval b) = true then true else false
  | Imply (a, b) -> if (eval a) = true && (eval b) = false then false else true
  | Equal (a, b) -> if (eval_exp a) = (eval_exp b) then true else false;;
