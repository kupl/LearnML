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
  | Minus of exp * exp

let rec eval_num : exp -> int
      = fun e ->
        match e with
          Plus(exp1, exp2) -> eval_num exp1 + eval_num exp2
          | Minus(exp1, exp2) -> eval_num exp1 - eval_num exp2
          | Num a -> a;;


let rec eval : formula -> bool
= fun f ->
  match f with
    True -> true
    | False -> false
    | Imply(f1', f2') ->
      if eval(f1') = true then
        if eval(f2') = true then true
        else false
      else true
    | Not f' ->
        if eval(f') = true then false else true
    | AndAlso(f1', f2') -> 
      if (eval(f1') = true) && (eval(f2') = true) then true else false
    | OrElse(f1', f2') -> 
      if (eval(f1') = true) || (eval(f2') = true) then true else false
    | Equal(exp1, exp2) -> eval_num(exp1) = eval_num(exp2);;

      
      