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

let rec evalexp (f: exp): int =
  match f with
    | Num a -> a
    | Plus (a, b) -> evalexp a + evalexp b
    | Minus (a, b) -> evalexp a - evalexp b

let rec eval (f: formula): bool =
  match f with
    | True -> true
    | False -> false
    | Not a -> if (eval a) = true then false else true
    | AndAlso (a, b) -> eval a && eval b
    | OrElse (a, b) -> eval a || eval b
    | Imply (a, b) -> not (eval a) || eval b
    | Equal (a, b) -> if (evalexp a) = (evalexp b) then true else false



  