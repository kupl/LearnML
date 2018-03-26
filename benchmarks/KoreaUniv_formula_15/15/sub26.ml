type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
= fun f -> match f with
  True-> true
| False-> false
| Neg for1 -> if(eval for1) then false else true
| Or(for1,for2) -> if(eval for1) then true else if(eval for2) then true else false
| And(for1,for2) -> if(eval for1=false) then false else if(eval for2=false) then false else true
| Imply(for1,for2) -> if(eval for1=false) then true else if(eval for2) then true else false
| Equiv(for1,for2) -> if(eval for1=eval for2) then true else false
(* Problem 5 *)
