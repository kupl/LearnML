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
  let rec calculate (f2 : exp) : int =
    match f2 with
    | Num i -> i
    | Plus (e1, e2) -> calculate e1 + calculate e2
    | Minus (e1, e2) -> calculate e1 - calculate e2
  in

  match f with
  | True -> true
  | False -> false
  | Not f -> not (eval f)
  | AndAlso (f1, f2) -> eval f1 && eval f2
  | OrElse (f1, f2) -> eval f1 || eval f2
  | Imply (f1, f2) -> if eval f1 = false || eval f2 = true then true else false
  | Equal (e1, e2) -> if calculate e1 = calculate e2 then true else false
