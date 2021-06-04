type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec calc (exp : exp) : int =
  match exp with
  | Num a -> a
  | Plus (a, b) -> calc a + calc b
  | Minus (a, b) -> ( match (a, b) with Num a, Num b -> a - b | _, _ -> 0 )


let rec eval (formula : formula) : bool =
  match formula with
  | True -> true
  | False -> false
  | Not a -> if eval a then false else true
  | AndAlso (a, b) -> if eval a then eval b else false
  | OrElse (a, b) -> if eval a then true else eval b
  | Imply (a, b) -> if eval a then eval b else true
  | Equal (a, b) -> if calc a = calc b then true else false
