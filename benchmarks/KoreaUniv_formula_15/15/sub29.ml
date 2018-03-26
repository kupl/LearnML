type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> 
  match f with
   True -> true
 | False -> false
 | Neg x -> if (eval x) then false else true
 | Or (x,y) -> (eval x) || (eval y)
 | And (x,y) -> (eval x) && (eval y)
 | Imply (x,y) -> let cx = (eval x) in let cy = (eval y) in if cx = true then if cy = true then true else false else true
 | Equiv (x,y) -> let cx = (eval x) in let cy = (eval y) in if cx = cy then true else false
