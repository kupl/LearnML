(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
	|[] -> 0
	|h::[]->h
	|h::t->
		let x = max t in
		if h > x then h else x;;

let rec min : int list -> int
= fun lst -> match lst with
	|[] -> 0
	|h::[] -> h
	|h::t -> 
		let x = min t in
		if h < x then h else x;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
	match lst with
		|[] ->[]
		|h::t -> match pred h with
							|true -> h::filter pred t
							|false -> filter pred t;;  

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
		Empty -> false
	|Node(a, b, c) -> 
		if a == n then true
		else if a < n then mem n b
		else mem n c;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
	|	ZERO -> n1
	|	SUCC a -> natadd (SUCC (n1)) a;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
	| ZERO -> ZERO
	|	SUCC k -> natadd n1 (natmul n1 k);;

(*********************)
(*     Problem 6     *)
(*********************)
type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec expf x = match x with 
	| Num n -> n
	| Plus (e1, e2) -> (expf e1) + (expf e2)
	| Minus (e1, e2) -> (expf e1) - (expf e2);;

let rec eval : formula -> bool
= fun f -> match f with
	|True -> true
	|False -> false
	|Not n -> (match eval n with
							|true -> false
							|false -> true)
	|AndAlso (n1, n2) -> (eval n1) && (eval n2)
	|OrElse (n1, n2) -> (match eval n1 with
													|true -> (match eval n2 with
																		|true -> true
																		|false -> true)
													|false -> (match eval n2 with
																		|true -> true
																		|false -> false))
	|Imply (n1, n2) -> (match eval n1 with
													|true -> (match eval n2 with
																		|true -> true
																		|false -> false)
													|false -> (match eval n2 with
																		|true -> true
																		|false -> true))
	|Equal (n1, n2) -> 
		let a1 = expf n1 in 
		let a2 = expf n2 in 
		if a1 == a2 then true else false;;
