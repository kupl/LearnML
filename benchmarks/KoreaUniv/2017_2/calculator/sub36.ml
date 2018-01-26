type exp = X
    | INT of int
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp

let rec calculator : exp -> int 
= fun exp -> 
  let rec substitute exp value =
    match exp with
    | X -> INT value 
    | INT i -> INT i
    | ADD (exp1, exp2) ->
        ADD ((substitute exp1 value), (substitute exp2 value))
    | SUB (exp1, exp2) ->
        SUB ((substitute exp1 value), (substitute exp2 value))
    | MUL (exp1, exp2) ->
        MUL ((substitute exp1 value), (substitute exp2 value))
    | DIV (exp1, exp2) ->
        DIV ((substitute exp1 value), (substitute exp2 value))
    | SIGMA (exp1, exp2, exp3) -> SIGMA(exp1, exp2, exp3)
  in
  let rec sigma f a b =
    if b < a
    then 0
    else (f a) + (sigma f (a+1) b)
  in
  match exp with
  | X -> raise (Failure "cannot calculate exp with variable")
  | INT i -> i
  | ADD (exp1, exp2) -> (calculator exp1) + (calculator exp2)
  | SUB (exp1, exp2) -> (calculator exp1) - (calculator exp2)
  | MUL (exp1, exp2) -> (calculator exp1) * (calculator exp2)
  | DIV (exp1, exp2) -> (calculator exp1) / (calculator exp2)
  | SIGMA (exp1, exp2, exp3) ->
    let eval = (function x -> calculator (substitute exp3 x)) in           
    sigma eval (calculator exp1) (calculator exp2)