type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec calc (e : exp) : int =
  match e with
  | Num i -> i
  | Plus (a, b) -> calc a + calc b
  | Minus (a, b) -> calc a - calc b


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not a -> eval a
  | AndAlso (a, b) -> if eval a = true then eval b else false
  | OrElse (a, b) -> if eval a = false then eval b else true
  | Imply (a, b) -> if eval a = true then eval b else true
  | Equal (a, b) -> if calc a = calc b then true else false
