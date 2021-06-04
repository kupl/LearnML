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
  | Num x -> x
  | Plus (x, y) -> eval_exp x + eval_exp y
  | Minus (x, y) -> eval_exp x - eval_exp y


let rec eval_formula (p : formula) : formula =
  match p with
  | True -> True
  | False -> False
  | Not x -> if eval_formula x = True then False else True
  | AndAlso (x, y) ->
      if eval_formula x = True && eval_formula y = True then True else False
  | Imply (x, y) -> if eval_formula x = False then True else eval_formula y
  | Equal (x, y) -> if eval_exp x = eval_exp y then True else False
  | OrElse (__s10, __s11) -> (
      match (eval_formula __s11, eval_formula __s10) with
      | False, False -> False
      | _, _ -> True )


let eval (x : formula) : bool = if eval_formula x = True then true else false
