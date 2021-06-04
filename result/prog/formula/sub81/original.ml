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
  | Plus (a, b) -> ( match (a, b) with Num a, Num b -> a + b | _, _ -> 0 )
  | Minus (a, b) -> ( match (a, b) with Num a, Num b -> a - b | _, _ -> 0 )


let rec eval (formula : formula) : bool =
  match formula with
  | True -> true
  | False -> false
  | Not a -> ( match a with True -> false | False -> true | _ -> false )
  | AndAlso (a, b) -> (
      match (a, b) with
      | True, True -> true
      | True, False -> false
      | False, True -> false
      | False, False -> false
      | _, _ -> false )
  | OrElse (a, b) -> (
      match (a, b) with
      | True, True -> true
      | True, False -> true
      | False, True -> true
      | False, False -> false
      | _, _ -> false )
  | Imply (a, b) -> (
      match (a, b) with
      | True, True -> true
      | True, False -> false
      | False, True -> true
      | False, False -> true
      | _, _ -> false )
  | Equal (a, b) -> if calc a = calc b then true else false
