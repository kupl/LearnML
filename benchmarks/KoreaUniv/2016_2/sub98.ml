(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
	let bigger a b = if a >= b then a else b
	in match lst with
	| [] -> 0
	| hd::tl -> List.fold_left (bigger) hd tl

let rec min : int list -> int
= fun lst -> 
	let smaller a b = if a <= b then a else b
	in match lst with
	| [] -> 0
	| hd::tl -> List.fold_left (smaller) hd tl

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
	match lst with
	| [] -> []
	| hd::tl -> if pred hd then [hd] @ filter pred tl else filter pred tl

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =
	f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
	| Empty -> false
	| Node (a, b, c) -> if n = a then true else mem n b || mem n c

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
	| ZERO -> n1
	| SUCC (n) -> SUCC (natadd n1 n)

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
	| ZERO -> ZERO
	| SUCC (n) -> natadd (natmul n1 n) n1

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
	match f with
	| True -> true
	| False -> false
	| Not f1 -> if eval f1 then false else true
	| AndAlso (f1, f2) -> if (eval f1) && (eval f2) then true else false
	| OrElse (f1, f2) -> if (eval f1) || (eval f2) then true else false
	| Imply (f1, f2) -> if eval f1 then eval f2 else true
	| Equal (e1, e2) -> let rec cal n = match n with
									| Num n1 -> n1
									| Plus (n1, n2) -> (cal n1) + (cal n2)
									| Minus (n1, n2) -> (cal n1) - (cal n2)
								in if (cal e1) = (cal e2) then true else false