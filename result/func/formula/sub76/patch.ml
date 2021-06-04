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
  | Not f1 -> if eval f1 then false else true
  | AndAlso (f1, f2) -> if eval f1 then eval f2 else false
  | OrElse (f1, f2) -> if eval f1 then true else eval f2
  | Imply (f1, f2) -> if eval f1 then eval f2 else true
  | Equal (exp1, exp2) ->
      let rec e2i (exp : exp) : int =
        match exp with
        | Num i -> i
        | Plus (e1, e2) -> e2i e1 + e2i e2
        | Minus (e1, e2) -> e2i e1 - e2i e2
      in
      e2i exp1 = e2i exp2
