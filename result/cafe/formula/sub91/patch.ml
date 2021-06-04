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
  let pre (a : formula) : formula = if eval a = true then True else False in

  let rec prem (a : exp) : exp =
    match a with
    | Num b -> Num b
    | Plus (Num b, Num c) -> Num (b + c)
    | Minus (Num b, Num c) -> Num (b - c)
    | Plus (b, c) -> prem (Plus (prem b, prem c))
    | Minus (b, c) -> prem (Minus (prem b, prem c))
  in

  match f with
  | True -> true
  | False -> false
  | Not a ->
      if a = True then false else if a = False then true else not (eval a)
  | AndAlso (a, b) ->
      if a = True && b = True then true
      else if a = False && b = True then false
      else if a = True && b = False then false
      else if a = False && b = False then false
      else eval (AndAlso (pre a, pre b))
  | OrElse (a, b) -> (
      match (a, b) with
      | False, False -> false
      | True, __s17 -> true
      | __s18, True -> true
      | __s19, __s20 -> eval (OrElse (pre b, pre __s19)) )
  | Imply (a, b) ->
      if a = True && b = True then true
      else if a = False && b = True then true
      else if a = True && b = False then false
      else if a = False && b = False then true
      else eval (Imply (pre a, pre b))
  | Equal (a, b) -> if prem a = prem b then true else false


let (_ : bool) = eval (Not (Not (Not False)))

let (_ : bool) = eval (AndAlso (True, Not False))

let (_ : bool) = eval (AndAlso (False, False))

let (_ : bool) = eval (AndAlso (True, False))

let (_ : bool) = eval (OrElse (True, Not False))

let (_ : bool) = eval (OrElse (False, False))

let (_ : bool) = eval (OrElse (True, False))

let (_ : bool) = eval (Imply (Imply (True, False), True))

let (_ : bool) = eval (Equal (Num 1, Plus (Num 1, Num 1)))

let (_ : bool) = eval (Equal (Num 2, Minus (Num 3, Num 1)))
