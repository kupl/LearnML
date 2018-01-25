(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l = match l with | [hd] -> hd | hd::tl -> f hd (fold f tl);;
let rec max : int list -> int = fun lst -> fold (fun x y -> if x > y then x else y) lst;;
let rec min : int list -> int = fun lst -> fold (fun x y -> if x < y then x else y) lst;;

(*********************)
(*     Problem 2     *)
(*********************)	
let rec filter pred lst = match lst with | [] -> [] | hd::tl -> if pred hd = true then hd::(filter pred tl) else filter pred tl;;

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

let rec mem : int -> btree -> bool = fun n tree -> match tree with | Empty -> false | Node (x, y, z) -> if n = x then true else (mem n y) || (mem n z);;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat = fun n1 n2 -> match n2 with | ZERO -> n1 | SUCC n3 -> natadd (SUCC n1) n3;;

let rec natmul : nat -> nat -> nat = fun n1 n2 -> match n2 with | ZERO -> ZERO | SUCC ZERO -> n1 | SUCC n3 -> natadd n1 (natmul n1 n3);;

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

let rec eval : formula -> bool = fun f -> match f with | True -> true | False -> false | Not x -> not(eval x) | AndAlso(x, y) -> (eval x) && (eval y) | OrElse(x, y) -> (eval x) || (eval y) | Imply(x, y) -> not(eval x) || (eval y) | Equal(x, y) -> x = y;;

