type formula = True
  |False
  |Not of formula
  |AndAlso of formula * formula
  |OrElse of formula * formula
  |Imply of formula * formula
  |Equal of exp * exp
and exp = Num of int
  |Plus of exp * exp
  |Minus of exp * exp
let rec eval_exp n =
  match n with
  |Num a -> a
  |Plus (left, right) -> (eval_exp left)+(eval_exp right)
  |Minus (left, right) -> (eval_exp left)-(eval_exp right)
let rec eval m =
  match m with
  |True -> true
  |False -> false
  |Not f -> not (eval f)
  |AndAlso (left, right) -> (eval left) && (eval right)
  |OrElse (left, right) -> (eval left) || (eval right)
  |Imply (left, right) -> (not (eval left)) || (eval right)
  |Equal (left, right) -> (eval_exp left) = (eval_exp right)
