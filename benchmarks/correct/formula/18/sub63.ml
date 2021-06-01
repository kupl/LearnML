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

let rec eval : formula -> bool
= fun f -> match f with
  | True -> true
  | False -> false
  | Not f1 -> if eval f1 = true then false else true
  | AndAlso (f1, f2) -> if eval f1 = true && eval f2 = true then true else false
  | OrElse (f1, f2) -> if eval f1 = true || eval f2 = true then true else false 
  | Imply (f1, f2) -> if eval f1 = true && eval f2 = false then false else true
  | Equal (e1, e2) -> let rec eval_exp : exp -> int
    = fun e -> match e with
      | Num x -> x
      | Plus (et1, et2) -> eval_exp et1 + eval_exp et2
      | Minus (et1, et2) -> eval_exp et1 - eval_exp et2 in
        if eval_exp e1 = eval_exp e2 then true else false;;