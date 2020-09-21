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

let rec mynum : exp -> int
= fun e ->
    match e with
      | Num e1 -> e1
      | Plus (e1, e2) -> (mynum e1) + (mynum e2)
      | Minus (e1, e2) -> (mynum e1) - (mynum e2);;

let rec eval : formula -> bool
= fun f ->
    match f with
      | True -> true
      | False -> false
      | Not p -> not (eval p)
      | AndAlso (p, q) -> (eval p) && (eval q)
      | OrElse (p, q) -> (eval p) || (eval q)
      | Imply (p, q) -> (not (eval p)) || (eval q)
      | Equal (e1, e2) -> if ((mynum e1) = (mynum e2)) then true else false;;

(* Test Cases
eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
eval (Equal (Num 3, Plus (Num 1, Num 2)));;
*)
