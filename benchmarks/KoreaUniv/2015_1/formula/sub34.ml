type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> (* TODO *)


 match a with
  True -> true
  |False -> false 
  |Neg(e1) -> if e1 == true then false else true
  |Or(e1,e2) -> if (e1 == true) && (e2 == true) then true
else if (e1 == true) && (e2 == false) then true
else if (e1 == false) && (e2 == true) then true
else if (e1 == false) && (e2 == false) then false
  |And(e1,e2) if (e1 == true) && (e2 == true) then true
else if (e1 == true) && (e2 == false) then false
else if (e1 == false) && (e2 == true) then false
else if (e1 == false) && (e2 == false) then false
  |Imply(e1,e2) if (e1 == true) && (e2 == true) then true
else if (e1 == true) && (e2 == false) then false
else if (e1 == false) && (e2 == true) then true
else if (e1 == false) && (e2 == false) then true
  |Equiv(e1,e2) -> if e1==e2 then true else false;; 
