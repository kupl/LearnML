(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
	if x = y then 1
	else if y = 0 then 1
	else pascal(x-1,y-1) + pascal(x-1,y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
	if a > b then 0 (* a must be LT b*)
	else if a = b then f a
	else f a + sigma f (a+1) b ;;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 
	match l with
	 [] -> 0 (*if empty list *)
	|[x] -> x
	|hd :: tl -> 
		if hd > max tl then hd
		else max tl;;

let rec min : int list -> int
=fun l -> 
	match l with
	 [] -> 0 (*if empty list *)
	|[x] -> x
	|hd :: tl -> 
		if hd < min tl then hd
		else min tl;;

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
	| Neg f -> eval f = false 
	| Or (f,f') -> eval f || eval f'
	| And (f,f') -> eval f && eval f'
	| Imply (f,f') -> eval f = false || eval f' 
	| Equiv (f,f') -> eval f = eval f' ;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
	match n1 with
	ZERO -> n2
	|SUCC (nat) -> SUCC (natadd nat n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
	match n1 with
	ZERO -> ZERO
	|SUCC (nat) -> natadd n2 (natmul nat n2);;