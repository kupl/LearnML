(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 0 (* TODO *)

let rec min : int list -> int
= fun lst -> 0 (* TODO *)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = [] (* TODO *)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = a (* TODO *)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> true (* TODO *)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> ZERO (* TODO *)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> ZERO (* TODO *)

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
= fun f -> true (* TODO *)

