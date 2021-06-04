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
  match f with
  | True -> true
  | False -> false
  | Not ev -> if ev = True then false else true
  | AndAlso (ev1, ev2) -> if ev1 = True && ev2 = True then true else false
  | OrElse (ev1, ev2) -> if ev1 = False && ev2 = False then false else true
  | Imply (ev1, ev2) -> if ev1 = True && ev2 = False then false else true
  | Equal (in1, in2) ->
      let rec pl_mi x =
        match x with
        | Num n -> n
        | Plus (n1, n2) -> pl_mi n1 + pl_mi n2
        | Minus (n1, n2) -> pl_mi n1 - pl_mi n2
      in
      if pl_mi in1 = pl_mi in2 then true else false
