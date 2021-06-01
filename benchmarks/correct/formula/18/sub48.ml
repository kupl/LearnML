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

let rec calculator = fun x ->
  match x with
    | Num a' -> a'
    | Plus (a', b') -> (calculator a'+calculator b')
    | Minus (a', b') -> (calculator a'-calculator b');;

let rec eval : formula -> bool
= fun f -> (*TODO*)
  match f with 
    | True -> true
    | False -> false
    | Not m -> if eval m then false else true 
    | AndAlso (m, n) -> if eval m && eval n then true else false
    | OrElse (m, n) -> if eval m || eval n then true else false
    | Imply (m, n) -> if eval m && not (eval n) then false else true
    | Equal (m, n) -> if calculator m = calculator n then true else false;;
    
eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
