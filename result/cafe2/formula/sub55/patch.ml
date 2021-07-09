type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec getInt (number : exp) : int =
  match number with
  | Num x -> x
  | Plus (a, b) -> getInt a + getInt b
  | Minus (a, b) -> getInt a - getInt b


let rec getExpression (f : exp) : exp =
  match f with
  | Num x -> Num x
  | Plus (a, b) -> Num (getInt (Plus (a, b)))
  | Minus (a, b) -> Num (getInt (Minus (a, b)))


let rec getValue (f : formula) : formula =
  match f with
  | True -> True
  | False -> False
  | Not x ->
      if x = True then False
      else if x = False then True
      else getValue (Not (getValue x))
  | AndAlso (x, y) ->
      if x = True && y = True then True
      else if x = False || y = False then False
      else getValue (AndAlso (getValue x, getValue y))
  | OrElse (x, y) -> (
      match (getValue x, getValue y) with False, False -> False | _ -> True )
  | Imply (x, y) ->
      if x = False then True
      else if x = True && y = True then True
      else if x = True && y = False then False
      else getValue (Imply (getValue x, getValue y))
  | Equal (ex1, ex2) ->
      if getExpression ex1 = getExpression ex2 then True else False


let rec eval (f : formula) : bool =
  if f = True then true else if f = False then false else eval (getValue f)
