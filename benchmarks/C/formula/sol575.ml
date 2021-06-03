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

let proc_imply =
  fun imp ->
    match imp with
    | (true, true) -> true
    | (true, false) -> false
    | (false, true) -> true
    | (false, false) -> true;;
    
let rec proc_exp = 
  fun x ->
    match x with
      | Plus(k1, k2) -> proc_exp k1 + proc_exp k2
      | Minus(k1, k2) -> proc_exp k1 - proc_exp k2
      | Num k -> k;;
      
let rec eval : formula -> bool
= fun f -> match f with
  | True -> true
  | False -> false
  | Not(k) -> not (eval(k))
  | AndAlso(k1,k2) -> eval(k1) && eval(k2)
  | OrElse(k1,k2) -> eval(k1) || eval(k2)
  | Imply(k1,k2) -> proc_imply (eval(k1), eval(k2))
  | Equal(k1,k2) -> proc_exp(k1) == proc_exp(k2);;
  
(*eval ((Imply (Imply (True, False), True)));;*)
(*eval (Not (Equal (Num 2, Plus (Num 1, Num 2))));;*)
