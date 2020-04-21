type formula = 
    True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp =
    Num of int
  | Plus of exp * exp
  | Minus of exp * exp


let rec eval f =
  let rec eval_num e =
    match e with
    | Num e -> e
    | Plus (e1, e2) -> eval_num e1 + eval_num e2
    | Minus (e1, e2) -> eval_num e1 - eval_num e2
  in
  match f with
  | True -> true
  | False -> false
  | Not fp -> not (eval fp)
  | AndAlso (fp1, fp2) -> (eval fp1) && (eval fp2)
  | OrElse (fp1, fp2) -> (eval fp1) || (eval fp2)
  | Imply (fp1, fp2) -> not (eval fp1) || (eval fp2)
  | Equal (e1, e2) -> (eval_num e1) = (eval_num e2)
