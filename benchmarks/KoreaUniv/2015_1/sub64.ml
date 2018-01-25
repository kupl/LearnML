(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
match (x,y) with 
(_,0) | (1,_) -> 1
|(_,_) ->
if x=y then 1
else pascal(x-1,y-1)+pascal(x-1,y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
if a = b then f a 
else (f a) + (sigma f (a+1) b);;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 
match l with
[a]-> a
|hd::tl-> 
if hd > (max tl) then hd
else max tl
|[]-> raise Not_found;;

let rec min : int list -> int
=fun l -> 
match l with
[a]-> a
|hd::tl-> 
if hd < (min tl) then hd
else min tl
|[]-> raise Not_found;;

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
=fun f ->
match f with
  True -> true 
| False -> false
| Neg f -> 
if (eval f) = true then false 
else true
| Or (f1, f2) ->
if (eval f1) = true then true
else (eval f2)
| And (f1, f2) ->
if (eval f1) = false then false 
else (eval f2)
| Imply (f1, f2) ->
if (eval f2) = true then true 
else if (eval f1) = false then true
else false
| Equiv (f1, f2) ->
(eval f1) = (eval f2);;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
match n1 with
ZERO -> n2
|SUCC n1 -> SUCC (natadd n1 n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
match n1 with
ZERO -> ZERO
|SUCC n1 -> natadd n2 (natmul n1 n2);;
