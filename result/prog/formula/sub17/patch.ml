type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec calc (ex : exp) : int =
  match ex with
  | Num f -> f
  | Plus (a, b) -> calc a + calc b
  | Minus (a, b) -> calc a - calc b


let rec eval (e : formula) : bool =
  match e with
  | Not x -> if eval x then false else true
  | AndAlso (a, b) -> if eval a then eval b else false
  | OrElse (a, b) -> if a = True then true else eval b
  | Imply (a, b) -> if eval a then eval b else true
  | Equal (a, b) -> calc a = calc b
  | False -> false
  | True -> true
