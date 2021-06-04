type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec getexp inpt =
  match inpt with
  | Num form -> form
  | Plus (form, lat) -> getexp form + getexp lat
  | Minus (form, lat) -> getexp form - getexp lat


let rec eval input =
  match input with
  | True -> true
  | False -> false
  | Not inpt -> not (eval inpt)
  | AndAlso (form, lat) -> eval form && eval lat
  | OrElse (form, lat) -> eval form || eval lat
  | Imply (form, lat) -> (not (eval form)) || eval lat
  | Equal (form, lat) -> getexp form = getexp lat
