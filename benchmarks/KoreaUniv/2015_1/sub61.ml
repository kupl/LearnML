(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) ->
if y = 0 then 1
  else if x = y then 1
  else pascal(x-1,y-1) + pascal(x-1,y);;
val pascal : int * int -> int = <fun>

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
if a<b then (sigma f (a+1) b) + (f a)
else (f a);;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 
match l with
[]-> 0
|[hd] -> hd
|hd::tl -> if (hd > max (tl)) then hd 
else (max (tl));;

let rec min : int list -> int
=fun l -> 
match l with
[]-> 0
|[hd] -> hd
|hd::tl -> if (hd < min tl) then hd 
else (min tl);;

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
	|False -> false
	|Neg (f1) -> not(eval f1)
	|Or (f1, f2) -> (eval f1) || (eval f2)
	|And (f1, f2) -> (eval f1) && (eval f2)
	|Imply (f1, f2) -> if (eval f1)=true then (eval f2)
	else true
	|Equiv (f1, f2) -> if (eval f1)=(eval f2) then true
	else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
match n1 with
ZERO -> n2
|SUCC (n) -> SUCC(natadd n n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
match n1 with
ZERO -> ZERO
|SUCC(n)->if n=ZERO then n2