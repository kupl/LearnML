pl_assign

(* 1 *)

let rec pascal : int * int -> int
=fun (x,y) -> 1
let rec pascal (x,y) =
  match (x,y) with 
  |(m,n) -> if m=n then 1
	else if m=0 then 1 else if n=0 then 1
	else pascal (m-1,n-1) + pascal (m-1,n);;


(* 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
let rec sigma f a b =
	if a = b then f b
	else if a > b then 0
	(* else if b > a then -( ( f a ) + (sigma f a+1 b) ) *)
	else if a < b then ( f a ) + (sigma f a+1 b);;	


(* 3 *)
let rec max : int list -> int
=fun l -> 1 
let rec max l = 
	match l with
	| [] -> 
	| [a] -> a
	| hd::tl -> if hd < max(tl) then max(tl) else hd;;
 	
let rec min : int list -> int
=fun l -> 1 
let rec min l = 
	match l with
	| [] -> 
	| [a] -> a
	| hd::tl -> if hd > min(tl) then min(tl) else hd;;


(* 4 *)
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

let eval formula =
	match formula with
	  Neg f -> not f
	| Or(f1, f2) -> f1 || f2
	| And(f1, f2) -> f1 && f2
	| Imply (f1, f2) -> not(f1 && not f2)
	| Equiv (f1, f2) ->  if f1 then f2 else not f2;;

(* 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun a b ->(* TODO *)
let rec natadd a, b =
	match b with
	| ZERO ->
		let rec renat n =
		match n with
		| ZERO -> ZERO
		| SUCC (n2) -> SUCC (renat n2)
		in
		renat a
	|SUCC (b2) -> SUCC ( natadd a b2 );;



let rec natmul : nat -> nat -> nat
=fun a b ->
	match b with
	| ZERO -> ZERO
	| SUCC (b2) -> natadd a (natmul a, b2);


