(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
	| [] -> 0
	| hd::tl -> if hd < max tl && tl == [] then max tl else hd

let rec min : int list -> int
= fun lst -> match lst with
	| [] -> 0
	| hd::tl -> if hd > min tl && tl == [] then min tl else hd

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with 
	| [] -> []
	| hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl

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
	| Node (m, l, r) -> if m = n then true else mem n l || mem n r

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
	| ZERO -> n1
	| SUCC m -> natadd (SUCC n1) m

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
	| ZERO -> ZERO
	| SUCC m -> natadd n1 (natmul n1 m)

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
	| Not f -> not (eval f)
	| AndAlso (a, b) -> if (eval a) = false then false else if (eval b) = false then false else true
	| OrElse (a, b) -> if (eval a) = true then true else if (eval b) = true then true else false
	| Imply (a, b) -> not (eval a) || (eval b)
	| Equal (a, b) ->
		let rec eval2 e = match e with
			| Num n -> n
			| Plus (a, b) -> eval2 a + eval2 b
			| Minus (a, b) -> eval2 a - eval2 b
		in eval2 a = eval2 b