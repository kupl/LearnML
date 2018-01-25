(*********************)
(*     Problem 1     *)
(*********************)

let rec max : int list -> int
= fun lst -> 
	match lst with
	| [] -> min_int
	| hd::tl-> if (hd > (max tl)) then hd else (max tl)

let rec min : int list -> int
= fun lst ->
	match lst with
	| [] -> max_int
	| hd::tl-> if (hd < (min tl)) then hd else (min tl)

(*********************)
(*     Problem 2     *)
(*********************)

let rec filter pred lst = 
	match lst with
	| [] -> []
	| hd::tl->if (pred hd) then hd::(filter pred tl) else (filter pred tl)

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
= fun n tree -> 
	match tree with
	| Empty -> false
	| Node(b,c,d) -> if n = b then true 
					 else if mem n c = true || mem n d = true then true
					 else false

(*********************)
(*     Problem 5     *)
(*********************)

type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n1 with
	 | ZERO -> n2
	 | SUCC(n) -> natadd n (SUCC(n2))

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
		match n1 with
		 | ZERO -> ZERO
		 | SUCC(n) -> natadd (natmul n n2) n2

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

let rec exee : exp -> int
= fun f ->
		match f with
		 | Num(f) ->f
		 | Plus (f1,f2) -> (exee f1) + (exee f2)
		 | Minus (f1,f2) -> (exee f1) - (exee f2)

let rec eval : formula -> bool
= fun f -> 
		match f with
		 | True -> true
		 | False -> false
		 | Not f -> not(eval f)
		 | AndAlso (n1, n2) -> if ((eval n1) = true && (eval n2) = true) then true else false
		 | OrElse (n1, n2) -> if ((eval n1) = false && (eval n2) = false) then false else true
		 | Imply (n1, n2) -> if ((eval n1) = true && (eval n2) = false) then false else true
		 | Equal (n1, n2) -> (exee n1) = (exee n2)


