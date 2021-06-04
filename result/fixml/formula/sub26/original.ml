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
 fun fml ->
  let rec calc exp =
    match exp with
    | Num n -> n
    | Plus (le, re) -> calc le + calc re
    | Minus (le, re) -> calc le - calc re
  in

  match fml with
  | True -> true
  | False -> false
  | Not f -> eval f
  | AndAlso (lf, rf) -> eval lf && eval rf
  | OrElse (lf, rf) -> eval lf || eval rf
  | Imply (lf, rf) -> if eval lf then eval rf else true
  | Equal (le, re) -> calc le = calc re
