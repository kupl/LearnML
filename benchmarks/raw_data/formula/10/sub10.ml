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

let rec eval f =
  match f with True -> true
  | False -> false
  | Not k -> if eval k = true then false else true
  | AndAlso (a, b) -> if (eval a) && (eval b) then true else false
  | OrElse (a, b) -> if (eval a) || (eval b) then true else false
  | Imply (a, b) -> if (eval a) = false || (eval b) = true then true else false
  | Equal (a, b) -> let rec eval_exp e =
  match e with Num i -> i
  | Plus (a, b) -> (eval_exp a) + (eval_exp b)
  | Minus (a, b) -> (eval_exp a) - (eval_exp b) in
  if (eval_exp a) = (eval_exp b) then true else false

let _ = (eval(Equal (Num 3, Minus(Num 7, Num 9))));;