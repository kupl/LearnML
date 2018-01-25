(* Problem 1 *)
let rec pascal : int * int -> int
= fun (x,y) -> 1;;

let rec pascal (x,y) =
		if y=0 then 1 else if y=x then 1
		else if y>x then raise (Failure "Impossible")
		else (pascal (x-1, y-1) + pascal (x, y-1));;


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 1;;

let rec sigma f a b =
		if a>b then raise (Failure "Impossible")
		else if a=b then (f a)
		else (f a) + sigma f (a+1) b;;


(* Problem 3 *)
let rec max : int list -> int
= fun l -> 1;;

let rec max l =
		match l with
		[] -> 0 |
		h::t -> if h>(max t) then h else (max t);;

let rec min : int list -> int
= fun l -> 1;;

let rec min l =
		match l with
		[] -> 0 |
		[a] -> a |
		h::t -> if h<(min t) then h else (min t);;


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
= fun f -> true;;

let rec eval f =
		match f with
		True -> true |
		False -> false |
		Neg (f1) -> if f1 = True then false else true |
		Or (f1, f2) -> if (f1 = True && f2 = True) then false else true |
		And (f1, f2) -> if (f1 = True && f2 = True) then true else false |
		Imply (f1, f2) -> if (f1 = True && f2 = False) then false else true |
		Equiv (f1, f2) -> if f1 = f2 then true else false;;


(* Problem 5 *)
type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> ZERO;;

let rec natadd n1 n2 =
		match n1 with
		ZERO -> n2 |
		SUCC (a) -> SUCC (natadd a n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> ZERO;;

let rec natmul n1 n2 =
		match n2 with
		ZERO -> ZERO |
		SUCC (a) -> natadd n1 (natmul n1 a);;


