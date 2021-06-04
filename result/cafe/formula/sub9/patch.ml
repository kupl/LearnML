type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval (form : formula) : bool =
  let rec cal (exp : exp) : int =
    match exp with
    | Num ex1 -> ex1
    | Plus (ex1, ex2) -> cal ex1 + cal ex2
    | Minus (ex1, ex2) -> cal ex1 - cal ex2
  in

  match form with
  | True -> true
  | False -> false
  | Not for1 -> not (eval for1)
  | AndAlso (for1, for2) -> eval for1 && eval for2
  | OrElse (for1, for2) -> eval for1 || eval for2
  | Imply (for1, for2) ->
      if eval for1 = false || eval for2 = true then true else false
  | Equal (e1, e2) -> cal e1 = cal e2
