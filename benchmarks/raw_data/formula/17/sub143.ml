type formula = True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
and exp = Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec expeval : exp -> int = fun e ->
  match e with
  | Num ex -> ex
  | Plus(ex, ey) -> expeval(ex)+expeval(ey)
  | Minus(ex, ey) -> expeval(ex)-expeval(ey)

let rec eval : formula -> bool = fun f ->
  match f with
  | True -> true
  | False -> false
  | Not(fx) -> if eval(fx) then false else true
  | AndAlso(fx, fy) -> eval(fx) && eval(fy)
  | OrElse(fx, fy) -> eval(fx) || eval(fy)
  | Imply(fx, fy) -> (match eval(fx) with
    | true -> if eval(fy) then true else false
    | false -> true)
  | Equal(ex, ey) -> expeval(ex) = expeval(ey)

(* TESTING FIELD BELOW *)

