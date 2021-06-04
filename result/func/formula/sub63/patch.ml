type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec arithmetic (f : exp) : int =
  match f with
  | Num num -> num
  | Plus (num1, num2) -> arithmetic num1 + arithmetic num2
  | Minus (num1, num2) -> arithmetic num1 - arithmetic num2


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not fm -> if eval fm then false else true
  | AndAlso (fm1, fm2) -> eval fm1 && eval fm2
  | OrElse (fm1, fm2) -> eval fm1 || eval fm2
  | Imply (fm1, fm2) -> if not (eval fm1) then true else eval fm2
  | Equal (exp1, exp2) -> arithmetic exp1 = arithmetic exp2
