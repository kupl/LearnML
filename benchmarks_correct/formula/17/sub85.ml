type exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
        
let rec inteval x = match x with
  | Num (a) -> a
  | Plus (a, b) -> inteval(a) + inteval(b)
  | Minus (a, b) -> inteval(a) - inteval(b)

type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

let rec eval x: bool = match x with
  | True -> true
  | False -> false
  | Not (a) -> if (eval a) then false else true 
  | AndAlso (a, b) -> if (eval a && eval b) then true else false
  | OrElse (a, b) -> if (not (eval a) && not (eval b)) then false else true
  | Imply (a, b) -> if (eval a && not (eval b)) then false else true
  | Equal (a, b) -> inteval a = inteval b 
