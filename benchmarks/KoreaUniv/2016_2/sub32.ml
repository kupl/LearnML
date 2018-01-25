(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
|[] -> 0
|h::[] -> h
|h::t -> if h > max t then h
				else max t

let rec min : int list -> int
= fun lst -> match lst with
|[] -> 0
|h::[] -> h
|h::t -> if h < min t then h
				else min t

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
|[] -> []
|h::t -> if (pred h) then h::(filter pred t)
				else (filter pred t)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
|Empty -> false
|Node (k, left, right) -> if n=k then true
												else mem n left || mem n right

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
|ZERO -> n2
|SUCC (n) -> SUCC (natadd n n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
|ZERO -> ZERO
|SUCC (n) -> natadd n2 (natmul n n2)

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

let rec eval : formula -> bool
= fun f ->

let rec calculate f2 = match f2 with
	|Num (i) -> i
	|Plus (e1, e2) -> calculate (e1) + calculate (e2)
	|Minus (e1, e2) -> calculate (e1) - calculate (e2)
in

match f with
	|True -> true
	|False -> false
	|Not (f)  -> if f=True then false else true
	|AndAlso (f1, f2) -> if (f1=True && f2=True) then true else false
	|OrElse (f1, f2) -> if (f1=True || f2=True) then true else false
	|Imply (f1, f2) -> if (f1=True && f2=False) then false else true
	|Equal (e1, e2) -> if calculate e1 = calculate e2
		then true else false
