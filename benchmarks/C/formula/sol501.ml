type formula = True 
             | False 
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp

and exp = Num of int
         | Plus of exp * exp
         | Minus of exp * exp

let rec calc (a : exp) : int =
match a with
  | Num a -> a
  | Plus (b, c) -> (calc b) + (calc c)
  | Minus (b, c) -> (calc b) - (calc c)

let rec eval (input : formula) : bool =
match input with
  | True -> true
  | False -> false
  | Not a ->
    (
      match eval a with
      | true -> false
      | false -> true
    )
  | AndAlso (a, b) ->
    (
      match eval a with
      | true -> eval b
      | false -> false
    )
  | OrElse (a, b) ->
    (
      match eval a with
      | true -> true
      | false -> eval b
    )
  | Imply (a, b) ->
    (
      match eval a with
      | true -> eval b
      | false -> true
    )
  | Equal (c, d) ->
      if (calc c) = (calc d) then true
      else false

