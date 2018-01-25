
(* Problem 1 *) let rec pascal : int * int -> int =fun (x,y) -> 1
let rec pascal (a,b) = 
	if b = 0 || a = b then 1 else
	pascal ( a-1, b-1 ) + pascal( a-1, b)
(* Problem 2 *) let rec sigma : (int -> int) -> int -> int -> int =fun f a b -> 1
let rec sigma f a b =
	if a = b then f b else
	f a + sigma f (a+1) b
(* Problem 3 *) let rec max : int list -> int =fun l -> 1
let rec max p = 
	match p with
	[t] -> t
	| hd::tl -> if hd > max tl then hd else
	max tl
let rec min : int list -> int =fun l -> 1
let rec min p =
	match p with
	[t] -> t
	| hd::tl -> if hd < min tl then hd else
	min tl
(* Problem 4 *) type formula = True | False | Neg of formula | Or of formula * formula | And of formula * formula | Imply of formula * formula | Equiv of formula * formula let rec eval : formula -> bool =fun f -> true
let rec eval p =
	match p with
	True -> true
	| False -> false
	| Neg p -> not (eval p)
	| Or (a, b) -> (eval a) || (eval b)
	| And (a, b) -> (eval a) && (eval b)
	| Imply (a, b) -> (eval a) && (eval (Neg b))
	| Equiv (a, b) -> (eval (Neg a)) && (eval b)
(* Problem 5 *) type nat = ZERO | SUCC of nat let rec natadd : nat -> nat -> nat =fun n1 n2 -> ZERO
let rec natadd a b =
	match a with	
	ZERO -> b
	| SUCC l -> SUCC (natadd l b)
let rec natmul : nat -> nat -> nat =fun n1 n2 -> ZERO
let rec natmul a b =
	match a with
	ZERO -> ZERO
	| (SUCC ZERO )-> b
	| SUCC l -> natadd b (natmul l b)