type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let imply v = match v with true, x -> x | false, x -> true

let rec calc e =
  match e with
  | Num n -> n
  | Plus (e1, e2) -> calc e1 + calc e2
  | Minus (e1, e2) -> calc e1 - calc e2


let rec eval : formula -> bool =
 fun f ->
  match f with
  | True -> true
  | False -> false
  | Not fm -> not (eval fm)
  | AndAlso (fm1, fm2) -> eval fm1 && eval fm2
  | OrElse (fm1, fm2) -> eval fm2 || eval fm1
  | Imply (fm1, fm2) -> imply (eval fm1, eval fm2)
  | Equal (e1, e2) -> calc e1 = calc e2
