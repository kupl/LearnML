type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval : formula -> bool =
 fun f ->
  match f with
  | True -> true
  | False -> false
  | Not a -> not (eval a)
  | AndAlso (a, b) -> eval a && eval b
  | OrElse (a, b) -> eval a || eval b
  | Imply (a, b) -> if eval a then eval b else true
  | Equal (a, b) -> a = b


let rec expr e =
  match e with
  | Num n -> n
  | Plus (e1, e2) -> expr e1 + expr e2
  | Minus (e1, e2) -> expr e1 - expr e2
