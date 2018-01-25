(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> let rec loop fir ilst =
	match ilst with
		| [] -> fir
		| h::t -> if h > loop fir t then h else loop fir t
	in match lst with
		| h::t -> loop h t
		| [] -> 0

let rec min : int list -> int
= fun lst -> let rec loop fir ilst =
	match ilst with
		| [] -> fir
		| h::t -> if h < loop fir t then h else loop fir t
	in match lst with
		| h::t -> loop h t
		| [] -> 0
(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
		| [] -> []
		| h::t -> if pred h then h::(filter pred t) else (filter pred t)

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
	| Empty -> false
	| Node (x,y,z)  -> if x = n then true else mem n y || mem n z

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
		| ZERO -> n2
		| SUCC x -> SUCC (natadd x n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let rec mul num = match num with
		| 0 -> ZERO
		| _ -> SUCC (mul (num - 1))
	in let rec cal a = match a with
		| ZERO -> 0
		| SUCC a -> 1 + cal a
	in mul ((cal n1) * (cal n2))

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
= fun f -> match f with
		| True -> true
		| False -> false
		| Not x -> not(eval x)
		| AndAlso (x,y) -> eval x && eval y
		| OrElse (x,y) -> eval x || eval y
		| Imply (x,y) -> not(eval x) || (eval y) 
		| Equal (x,y) -> let rec value a = match a with
																		| Num x -> x
																		| Plus (x, y) -> value x + value y
																		| Minus (x, y) -> value x - value y
											in value x = value y

