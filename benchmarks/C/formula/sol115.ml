type formula = 
 True
|False
|Not of formula
|AndAlso of formula * formula
|OrElse of formula * formula
|Imply of formula * formula
|Equal of exp * exp

and exp = Num of int
|Plus of exp * exp
|Minus of exp * exp

let rec __eval_exp e =
  match e with
  Num a -> a
  |Plus (a,b) -> (__eval_exp a) + (__eval_exp b)
  |Minus (a,b) -> (__eval_exp a) - (__eval_exp b)

let rec eval f =
  match f with

  |True -> true
  |False -> false
  |Not nf -> not (eval nf)
  |AndAlso (f1, f2) -> (eval f1) && (eval f2)
  |OrElse (f1, f2) -> (eval f1) || (eval f2)
  |Imply (f1, f2) -> (not (eval f1)) || (eval f2)
  |Equal (e1, e2) -> if (__eval_exp e1) = (__eval_exp e2) then true else false
