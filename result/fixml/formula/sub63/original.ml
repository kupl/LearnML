type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec arithmetic f =
  match f with
  | Num num -> num
  | Plus (num1, num2) -> arithmetic num1 + arithmetic num2
  | Minus (num1, num2) -> arithmetic num1 - arithmetic num2


let rec eval : formula -> bool =
 fun f ->
  match f with
  | True -> true
  | False -> false
  | Not fm -> if eval fm then false else true
  | AndAlso (fm1, fm2) -> eval fm1 && eval fm2
  | OrElse (fm1, fm2) -> eval fm1 || eval fm2
  | Imply (fm1, fm2) ->
      if eval fm1 then if eval fm2 then true else false else false
  | Equal (exp1, exp2) -> arithmetic exp1 = arithmetic exp2
