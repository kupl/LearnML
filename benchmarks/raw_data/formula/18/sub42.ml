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
  
let rec oper : exp -> int
= fun f -> match f with
  | Num a -> a
  | Plus (a,b) -> (oper a) + (oper b)
  | Minus (a,b) -> (oper a) - (oper b)

let rec eval : formula -> bool
= fun f -> match f with
  | True -> true
  | False -> false
  | Not x -> not (eval x)
  | AndAlso (x,y) -> (eval x) && (eval y)
  | OrElse (x,y) -> (eval x) || (eval y)
  | Equal (a,b) -> if oper a = oper b then true else false
  | Imply (x,y) -> match (eval x,eval y) with
    | (false,false) -> true
    | (false,true) -> true
    | (true,false) -> false
    | (true,true) -> true;;

(*
eval (Imply (Imply (True, False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
*)