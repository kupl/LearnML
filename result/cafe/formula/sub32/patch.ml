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
  | Not a -> if eval a then false else true
  | AndAlso (left, right) -> if eval left && eval right then true else false
  | OrElse (left, right) -> if eval left || eval right then true else false
  | Imply (left, right) ->
      if eval left = false || eval right = true then true else false
  | Equal (left, right) ->
      let rec env (v : exp) : int =
        match v with
        | Num a -> a
        | Plus (a, b) -> env a + env b
        | Minus (a, b) -> env a - env b
      in
      if env left = env right then true else false
