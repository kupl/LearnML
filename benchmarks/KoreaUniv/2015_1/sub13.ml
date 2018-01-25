(* Problem 1 *)
let pascal : int * int -> int
= fun (x,y) -> 1;;
let rec pascal (x,y)  = 
if (x < y) then raise (Failure "ERROR : Second number cannot be larger than First one.")
else if ( y=0 || x=y ) then 1 
else ((pascal (x-1,y-1)) + (pascal (x-1,y)));;


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 1;;
let rec sigma f x y = if (x < y) then (f x + (sigma f (x+1) y))
else f x;;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 1;;
let rec max l =	match l with
	| [] -> raise(Failure "Too short")
	| hd :: tl -> if (tl = []) then hd
				else if ( hd <= max tl) then max tl
				else hd;;


let rec min : int list -> int
=fun l -> 1;;
let rec min l =	match l with
	| [] -> raise(Failure "Too short")
	| hd :: tl -> if (tl = []) then hd
				else if ( hd <= min tl) then hd
				else min tl;;


(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula;;

let rec eval : formula -> bool
=fun f -> true;;

let rec eval what_formula = match what_formula with
|True -> true
|False -> false
| Neg formula -> if (formula = True) then false else true
| Or (formula1, formula2) -> if (formula1 = False && formula2 = False) then false else true
| And (formula1, formula2) -> if (formula1 = True && formula2 = True) then true else false
| Imply (formula1, formula2) -> if (formula1 = True && formula2 = False) then false else true
| Equiv (formula1, formula2) -> if (formula1 = formula2) then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat;;
let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO;;
let rec natadd n1 n2 = match n2 with
ZERO -> n1
|SUCC (nat) -> SUCC (natadd n1 nat);;

let rec natmul : nat -> nat -> nat 
=fun n1 n2 -> ZERO;;
let rec natmul n1 n2 = match n2 with
| ZERO -> ZERO
| SUCC (nat) -> if n2 = ZERO then ZERO
else natadd n1 (natmul n1 nat);;