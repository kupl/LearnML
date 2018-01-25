(*********************)
(*     Problem 1     *)
(*********************)
let comp1 x y = if x >=  y then x else y;;
let comp2 x y = if x <=  y then x else y;;
let rec fold f l a =
		match l with
		| [] -> a
		| hd::tl -> f hd (fold f tl a)
let rec max : int list -> int
= fun lst -> fold comp1 lst (-100000000)
let rec min : int list -> int
= fun lst -> fold comp2 lst 100000000
(*********************)
(*     Problem 2     *)
(*********************)
let ans  = [];;
let rec filter pred lst =
		match lst with
		| [] -> ans
		| hd::tl -> if pred hd then hd::filter pred tl
							  else filter pred tl
(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
		begin
		f (f a)
		end

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
			match tree with
			|Empty ->  false
			|Node(k,t1,t2) -> if k = n then true
			else if k<n then mem n t2
			else mem n t1

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
			match n1 with
			|ZERO -> n2
			|SUCC(k) -> SUCC(natadd k n2)
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
			match n1 with
			|ZERO -> ZERO
			|SUCC(k) -> natadd n2 (natmul k n2)

(*********************)
(*     Problem 6     *)
(*********************)
type formula =
	|	True
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
let rec va e =
		match e with
		|Num k -> k
		|Plus(k1,k2) -> va k1 + va k2
		|Minus(k1,k2) -> va k1 - va k2
let rec eval : formula -> bool
=  fun f ->
      match f with
      |True -> true
      |False -> false
      |Not (k) -> not(eval k)
      |AndAlso (k1,k2) -> if eval k1 = false  then false else eval k2
      |OrElse (k1,k2) -> if eval k1 = true then true else eval k2
      |Imply (k1,k2) -> not(eval k1) || eval k2
      |Equal (k1,k2) -> va k1 = va k2
