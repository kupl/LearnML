type formula =
True
| False
| Neg of formula
| Or of formula * formula
| And of formula * formula
| Imply of formula * formula
| Equiv of formula * formula;;

(*
let eval formula = 
match formula with
True -> true
|False -> false
| Neg x -> if x = False then true else false
| Or (x,y) -> if (x = False && y = False) then false else true 
| And (x,y) -> if (x = True) && (y = True) then true else false
| Imply (x,y) -> if (x = True) && (y = False) then false else true
| Equiv (x,y) -> if x = y then true else false;;
*)

let rec eval formula = 
match formula with
True -> true
|False -> false
| Neg x -> not (eval x)
| Or (x,y) -> (eval x) || (eval y)
| And (x,y) -> (eval x) && (eval y)
| Imply (x,y) -> not (eval x) || (eval y)
| Equiv (x,y) -> if eval x = eval y then true else false;;
