type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not a -> not (eval a)
  | AndAlso (a, b) -> eval a && eval b
  | OrElse (a, b) -> eval a || eval b
  | Imply (a, b) -> if eval a = false || eval b = true then true else false
  | Equal (a, b) ->
      let rec evalexp (e : exp) : int =
        match e with
        | Num a -> a
        | Plus (a, b) -> evalexp a + evalexp b
        | Minus (a, b) -> evalexp a - evalexp b
      in
      if evalexp a = evalexp b then true else false
