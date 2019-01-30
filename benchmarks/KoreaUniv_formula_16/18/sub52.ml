type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp 
and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp;;

(*It's giving me the following error:*)
(*  "eval.ml line 32, characters 10-13:*)
(*   Error: This variant pattern is expected to have type formula*)
(*       The constructor Num does not belong to type formula"*)
(*I guess you can't do matching on variants beloning to two different types *)
(*(even though they are united by 'and'), and I have no idea what the correct*)
(*syntax should be here.*)

let eval f = 
  fun f ->
    let rec eval_inner f = 
      match f with 
        | True -> true
        | False -> false
        | Not (form) -> if eval_inner(form) then false else true
        | AndAlso (formula1, formula2) ->
          if (eval_inner(formula1) && eval_inner(formula2)) then true
          else false
        | OrElse (formula1, formula2) -> 
          if eval_inner(formula1) || eval_inner(formula2) then true
          else false
        | Imply (formula1, formula2) -> 
          if (eval_inner(formula1) == false) ||
          (eval_inner(formula1) && eval_inner(formula2)) then true
          else false
        | Equal (e1, e2) -> if (eval_inner(e1) == eval_inner(e2)) then true else false
        | Num num -> num
        | Plus (e1, e2) ->
          (eval_inner(e1) + eval_inner(e2))
        | Minus (e1, e2) ->
          (eval_inner(e1) - eval_inner(e2))
    in eval_inner f;;
    
eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
