type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval : formula -> bool =
 fun f ->
  let pre a = if eval a = true then True else False in

  let rec prem a =
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
  | OrElse (a, b) ->
      if a = True && b = True then true
      else if a = False && b = True then true
      else if a = True && b = False then true
      else if a = False && b = False then false
      else eval (AndAlso (pre a, pre b))
  | Imply (a, b) ->
      if a = True && b = True then true
      else if a = False && b = True then true
      else if a = True && b = False then false
      else if a = False && b = False then true
      else eval (Imply (pre a, pre b))
  | Equal (a, b) -> if prem a = prem b then true else false


let _ = eval (Not (Not (Not False)))

let _ = eval (AndAlso (True, Not False))

let _ = eval (AndAlso (False, False))

let _ = eval (AndAlso (True, False))

let _ = eval (OrElse (True, Not False))

let _ = eval (OrElse (False, False))

let _ = eval (OrElse (True, False))

let _ = eval (Imply (Imply (True, False), True))

let _ = eval (Equal (Num 1, Plus (Num 1, Num 1)))

let _ = eval (Equal (Num 2, Minus (Num 3, Num 1)))
