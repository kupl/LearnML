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
  | Not ev -> if eval ev then false else true
  | AndAlso (ev1, ev2) -> if eval ev1 then eval ev2 else false
  | OrElse (ev1, ev2) -> if eval ev1 then true else eval ev2
  | Imply (ev1, ev2) -> if eval ev1 then eval ev2 else true
  | Equal (in1, in2) ->
      let rec pl_mi (x : exp) : int =
        match x with
        | Num n -> n
        | Plus (n1, n2) -> pl_mi n1 + pl_mi n2
        | Minus (n1, n2) -> pl_mi n1 - pl_mi n2
      in
      if pl_mi in1 = pl_mi in2 then true else false
