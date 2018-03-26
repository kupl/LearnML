type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> true

let rec eval l = 
 match l with
  True -> true
 |False -> false
 |Neg l -> if(eval l) = true then false else true
 |Or(l1,l2) -> if( (eval l1)=true || (eval l2)=true ) then true else false
 |And(l1,l2) -> if( (eval l1)=true && (eval l2) = true ) then true else false
 |Imply(l1,l2)-> if( (eval l1)=true && (eval l2)=false) then false else true
 |Equiv(l1,l2)-> if( (eval l1)=(eval l2) ) then true else false;;

