type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval_exp (e : exp) : int =
  match e with
  | Num num -> num
  | Plus (n1, n2) -> eval_exp n1 + eval_exp n2
  | Minus (n1, n2) -> eval_exp n1 - eval_exp n2


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not v -> not (eval v)
  | AndAlso (v1, v2) -> eval v1 && eval v2
  | OrElse (v1, v2) -> eval v1 || eval v2
  | Imply (v1, v2) ->
      if eval v1 = false || eval v2 = true then true else eval v2
  | Equal (e1, e2) -> if eval_exp e1 = eval_exp e2 then true else false
