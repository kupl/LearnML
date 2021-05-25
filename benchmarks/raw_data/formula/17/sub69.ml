type exp = Num of int
          | Plus of exp * exp
          | Minus of exp * exp

type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp


let rec exp_to_int exp =
  match exp with
  | Num i -> i
  | Plus (a, b) -> (exp_to_int a) + (exp_to_int b)
  | Minus (a, b) -> (exp_to_int a) - (exp_to_int b)

let rec eval fml =
  match fml with
  | True -> true
  | False -> false
  | Not a -> if (eval a) then false else true
  | AndAlso (a, b) -> if ((eval a) && (eval b)) then true else false
  | OrElse (a, b) -> if ((eval a) || (eval b)) then true else false
  | Imply (a, b) -> if (not(eval a) || (eval b)) then true else false
  | Equal (a, b) -> if ((exp_to_int a) = (exp_to_int b)) then true else false

