(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1 (* TODO *)

if x < y then 0
else if x == y then 1
else if x == 0 then 1
else if  y== 0 then 1
else (pascal (x-1) y) + (pascal (x-1) (y-1));;


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 1 (* TODO *)


if a == b then f a
else f a + sigma f (a+1) b;;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 1 (* TODO *)

match l with
[] -> 0
|hd::tl -> if hd > (max tl) then hd
else  (max tl);;

let rec min : int list -> int
=fun l -> 1 (* TODO *)

match l with
[] -> (max l)
|hd::tl -> if(hd < (min tl)) && (min tl != 0) then hd
else if (min tl) != 0 then (min tl)
else 777;;

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
=fun f -> true (* TODO *)


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

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO(* TODO *)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO (* TODO *)