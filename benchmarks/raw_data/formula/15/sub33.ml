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


let rec eval f =
  match f with
    True -> true
  | False -> false
  | Not(subForm) -> not (eval subForm)
  | AndAlso(subForm1, subForm2) -> eval subForm1 && eval subForm2
  | OrElse(subForm1, subForm2) -> eval subForm1 || eval subForm2
  | Imply(subForm1, subForm2) -> not (eval subForm1) || eval subForm2
  | Equal(subExpr1, subExpr2) ->
      let rec calc e =
        match e with
          Num n -> n
        | Plus (subExpr1, subExpr2) -> calc subExpr1 + calc subExpr2
        | Minus (subExpr1, subExpr2) -> calc subExpr1 - calc subExpr2
      in
      calc subExpr1 = calc subExpr2
