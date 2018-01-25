let rec pascal : int * int -> int
=fun (x,y) -> 1;;
let rec pascal (x,y)=
   if x=y then 1 else if y=0 then 1 else pascal (x-1,y-1)+pascal (x-1,y);;


let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 1 ;;
let rec sigma f a b=
if a=b then (f a) else (f a) + (sigma f (a+1) b);;

let rec max : int list -> int
=fun l -> 1;;
let rec max l = 
   match l with 
       [x]->x
      | hd::tl-> if hd > (max tl) then hd else (max tl);;
let rec min : int list -> int
=fun l -> 1;;
let rec min l =
   match l with
       [x]->x
      |hd::tl-> if hd <(min tl) then hd else (min tl);;

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


type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO


let rec natadd n1 n2=
   match n1 with
      |ZERO->n2
      |SUCC(nat)->SUCC(natadd nat n2);;   


let rec natmul n1 n2=
   match n1 with
      |ZERO->ZERO
      |SUCC(ZERO)->n2
      |SUCC(nat)->natadd (natmul nat n2) n2;;



   