(* Problem 1 *) let rec pascal : int * int -> int =fun (x,y) -> 1 


let rec pascal (x,y) =
if x=0||y=0||x=y then 1
else pascal(x-1,y) + pascal(x-1,y-1)

 
(* Problem 2 *) let rec sigma : (int -> int) -> int -> int -> int =fun f a b -> 1 


let rec sigma f a b =
if a= b then b
else (f b) + sigma f a (b-1) ;;



(* Problem 3 *) let rec max : int list -> int =fun l -> 1 (* TODO *) let rec min : int list -> int =fun l -> 1 


let rec max l =
match l with
[x] -> x
lhd::tl->
if(hd > max(tl)) then hd
else max tl
|[] -> 0;;


let rec min l =
match l with
[x] -> x
lhd::tl->
if(hd < min(tl)) then hd
else min tl
|[] -> 0;;


(* Problem 4 *) type formula = True | False | Neg of formula | Or of formula * formula | And of formula * formula | Imply of formula * formula | Equiv of formula * formula let rec eval : formula -> bool =fun f -> true



let rec eval f =
match f with
True -> true
|False -> false
|Neg f -> 
if x=True then true then false else true
|Or (x,y) -> 
if x = True then true else if y=True then true else false
|And (x,y) ->
if x=True then eval y else false 
|Imply (x,y) ->
if x = y then false else true 
|Equiv(x,y) ->
if x = y then true else false;;

(* Problem 5 *) type nat = ZERO | SUCC of nat let rec natadd : nat -> nat -> nat =fun n1 n2 -> ZERO(* TODO *) let rec natmul : nat -> nat -> nat =fun n1 n2 -> ZERO


let rec natadd nat1 nat2 =  
match nat1 with
ZERO -> nat2
| SUCC nat3 -> SUCC (natadd nat3 nat2);;

let rec natmul nat1 nat2 = 
match (nat1, nat2) with
(ZERO,_)->ZERO
|(_,ZERO)->ZERO
|(SUCC ZERO,_) -> nat2
|(_, SUCC ZERO) -> nat1
|(SUCC nat3, nat2) -> natadd(natmul nat3 nat2) nat2;;


 