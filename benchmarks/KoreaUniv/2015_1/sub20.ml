(* Problem 1 *)
let pascal : int * int -> int
=fun (x,y) -> 1

let pascal(a,b) =
 if b=0 then 1
 else if a=b then 1
 else pascal(a-1,b-1) + pascal(a-1,b);;

 
(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 1

let rec sigma f a b = 
 if a=b then f a
 else f b + sigma f a (b-1);;

 
(* Problem 3 *)
let rec max : int list -> int
=fun l -> 1

let rec max l = 
 match l with
 [] -> -1
 | [a] -> a
 | h::t -> if h>max t then h else max t;;

let rec min : int list -> int
=fun l -> 1 

let rec min l =
 match l with
 [] -> -1
 | [a] -> a
 | h::t -> if h<min t then h else min t;;


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

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO(* TODO *)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO (* TODO *)
