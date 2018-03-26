type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula;;

let rec eval : formula -> bool
=fun f -> true ;;

let rec eval f=
   match f with
   True->true
   |False->false
   |Neg(f1)->if (eval f1) = false then true else false
   |Or(f1,f2)->if ((eval f1)||(eval f2))=true then true else false
   |And(f1,f2)->if ((eval f1)&&(eval f2))=true then true else false
   |Imply(f1,f2)->if ((eval f1)&&(eval f2))=true then true else if (eval f1)=false then true else false
   |Equiv(f1,f2)->if(eval f1)=(eval f2) then true else false;;





   