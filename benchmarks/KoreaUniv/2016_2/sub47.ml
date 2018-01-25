(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> (
  match lst with
  |h::[] -> h
  |h::t -> find h t)
  and find x y =
  match y with
  |[] -> x
  |h::t -> if x <= h then find h t else find x t
;;

let rec min : int list -> int
= fun lst -> (
match lst with
  |h::[] -> h
  |h::t -> find h t)
  and find x y =
  match y with
  |[] -> x
  |h::t -> if x >= h then find h t else find x t
;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
  match lst with
  | [] -> []    
  | h::t -> if pred h then h :: filter pred t else filter pred t;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f ( f a);;

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
= fun n1 n2 -> (
 match n1 with
  |ZERO -> n2
  |SUCC (x) -> SUCC (natadd x n2)
);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (
  match n1 with
  |ZERO -> ZERO
  |SUCC(x) -> natadd n2 (natmul x n2)
);;

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

