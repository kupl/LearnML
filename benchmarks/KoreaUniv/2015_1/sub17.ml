(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if x<y||y<0 then 0 else if x=0||x=y then 1 else pascal(x-1,y-1)+pascal(x-1,y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->if a=b then f b else f b+sigma f a (b-1);;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
| hd::[]-> hd
| hd::tl-> if hd > max tl then hd else max tl;;

let rec min : int list -> int
=fun l -> match l with
| hd::[]-> hd
| hd::tl-> if hd < min tl then hd else min tl;;

(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> match f with
| True -> true | False -> false | 
Neg True->false | Neg False -> true |
Or (True,True)->true |Or (True,False)->true | Or(False,True)->true| Or(False,False)->false
|And (True,True)->true | And(True,False)->false | And(False,True)->false | And(False,False)->false
|Imply (True,True)->true | Imply(True,False)->false | Imply(False,True)->true | Imply(False,False)->true
|Equiv (True,True)->true | Equiv(True,False)->false | Imply(False,True)->false| Imply(False,False)->true;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with 
|ZERO -> n2
|SUCC s -> natadd s (SUCC n2);; 

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
|ZERO -> ZERO 
|SUCC ZERO -> n2
|SUCC s -> natmul s (natadd n2 n2);; 