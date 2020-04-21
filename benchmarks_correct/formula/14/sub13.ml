exception EvalError

type exp = Num of int
          | Plus of exp * exp
          | Minus of exp * exp

type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp

let rec eval: formula -> bool =
  fun f ->
    let rec eval_exp: exp -> exp =
      fun e ->
        match e with
        | Num n ->
            Num n
        | Plus (Num n1, Num n2) ->
            Num (n1 + n2)
        | Plus (e1, e2) ->
            eval_exp (Plus ((eval_exp e1), (eval_exp e2)))
        | Minus (Num n1, Num n2) ->
            Num (n1 - n2)
        | Minus (e1, e2) ->
            eval_exp (Minus ((eval_exp e1), (eval_exp e2))) in
    match f with
    | True ->
        true
    | False ->
        false
    | Not f1 ->
        not (eval f1)
    | AndAlso (f1, f2) ->
        (eval f1) && (eval f2)
    | OrElse (f1, f2) ->
        (eval f1) || (eval f2)
    | Imply (f1, f2) ->
        (not (eval f1)) || (eval f2)
    | Equal (e1, e2) ->
        match ((eval_exp e1), (eval_exp e2)) with
        | (Num n1, Num n2) ->
            n1 = n2
        | _ ->
            raise EvalError
