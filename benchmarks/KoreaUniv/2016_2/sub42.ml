(* Name : Jungwon Seo / Student ID : 2012210051 *) 

exception Problem

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
 match lst with
 | [] -> raise Problem
 | [hd] -> hd
 | hd::tl ->
 if hd > max tl then hd
 else max tl

let rec min : int list -> int
= fun lst ->
 match lst with
 | [] -> raise Problem
 | [hd] -> hd
 | hd::tl ->
 if hd < min tl then hd
 else min tl

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter : ('a -> bool) -> 'a list -> 'a list
= fun pred lst ->
 match lst with
 | [] -> []
 | hd::tl ->
 if pred hd then hd::(filter pred tl)
 else filter pred tl

(*********************)
(*     Problem 3     *)
(*********************)
let rec double : ('a -> 'a) -> 'a -> 'a
= fun f a -> f (f a)

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
| Node (m, Empty, Empty) -> n=m
| Node (m, a, Empty) -> n=m || mem n a
| Node (m, Empty, b) -> n=m || mem n b
| Node (m, a, b) -> n=m || mem n a || mem n b

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
 | SUCC x -> natadd x (SUCC n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
 match n1 with
 | ZERO -> ZERO
 | SUCC ZERO -> n2
 | SUCC x ->  natmul x (natadd n2 n2)

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

let rec eval_exp : exp -> int
= fun e ->
 match e with
  Num a -> a
| Plus(a, b) -> eval_exp(a) + eval_exp(b)
| Minus(a, b) -> eval_exp(a) - eval_exp(b)
 
let rec eval : formula -> bool
= fun f ->
 match f with
 | True -> true
 | False -> false
 | Not a -> not (eval a)
 | AndAlso (a, b) -> eval a && eval b
 | OrElse (a, b) -> eval a || eval b
 | Imply(a, b) -> if (eval a)=false then true
                  else eval b
 | Equal(a, b) -> eval_exp(a)=eval_exp(b)
