(* Problem 1 *) let rec pascal : int * int -> int =fun (x,y) -> 

if (y=0) || (x=y) then 1
else pascal (x-1, y-1) + pascal (x-1 ,y);; 

(* Problem 2 *) let rec sigma : (int -> int) -> int -> int -> int =fun f a b ->


if a > b then 0
else f a + sigma f (a+1) b;;


(* Problem 3 *) let rec max : int list -> int =fun l -> 

match l with 
|[]->0
|hd::tl -> lim isbig 0 l;;



		 let rec min : int list -> int =fun l ->  

match l with 
|[]->0
|hd::tl -> lim issmall 10 l ;;

let rec lim f return l =
match l with 
|[]->return 
|hd::tl -> lim f (f return hd) tl;;

let isbig x y =
if x>y then x
else y;;

let issmall x y =
if x>y then y
else x;;


(* Problem 4 *) type formula = True | False | Neg of formula | Or of formula * formula | And of formula * formula 
| Imply of formula * formula | Equiv of formula * formula let rec eval : formula -> bool =fun f -> true (* TODO *) 

(* Problem 5 *) type nat = ZERO | SUCC of nat let rec natadd : nat -> nat -> nat =fun n1 n2 -> ZERO
(* TODO *) 
let rec natmul : nat -> nat -> nat =fun n1 n2 -> ZERO (* TODO *) 