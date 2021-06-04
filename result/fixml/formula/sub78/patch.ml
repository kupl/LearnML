type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec plusminus x =
  match x with
  | Plus (a, b) -> plusminus a + plusminus b
  | Minus (a, b) -> plusminus a - plusminus b
  | Num a -> a


let rec eval f =
  match f with
  | False -> false
  | True -> true
  | Not value -> not (eval value)
  | AndAlso (value, value1) -> (
      match value with
      | True -> (
          match value1 with True -> true | False -> false | _ -> eval value1 )
      | False -> false
      | _ -> eval value )
  | OrElse (value, value1) -> (
      match value with
      | True -> true
      | False -> (
          match value1 with True -> true | False -> false | _ -> eval value1 )
      | _ -> eval value )
  | Imply (value, value1) -> (
      match value with
      | True -> (
          match value1 with False -> false | True -> true | _ -> eval value1 )
      | False -> true
      | _ -> eval value = eval value1 )
  | Equal (value, value1) -> (
      match (value, value1) with
      | a, b -> if plusminus a = plusminus b then true else false )
