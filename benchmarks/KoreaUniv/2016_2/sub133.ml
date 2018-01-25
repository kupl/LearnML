(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
	let rec fold f l a =
		match l with
		| [] -> a
		| hd::tl -> f hd (fold f tl a) in
	fold (fun x y -> if x > y then x else y) lst min_int;;

let rec min : int list -> int
= fun lst -> 
	let rec fold f l a =
		match l with
		| [] -> a
		| hd::tl -> f hd (fold f tl a) in
	fold (fun x y -> if x > y then y else x) lst max_int;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
	| [] -> []
	| hd::tl -> (if pred hd then [hd] else []) @ filter pred tl;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = (f (f a));;

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
	| Node(value, left, right) -> 
		if(value = n) then true 
		else mem n left || mem n right ;;

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
	| SUCC(n) -> natadd n (SUCC (n2));;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	match n1 with
	| ZERO -> ZERO
	| SUCC(n) -> natadd n2 (natmul n n2);;

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
	let rec arithmatic e = 
		match e with
		| Num(e) -> e
		| Plus(e1, e2) -> arithmatic e1 + arithmatic e2
		| Minus(e1, e2) -> arithmatic e1 - arithmatic e2 in
	match f with
	| True -> true
	| False -> false
	| Not(f) -> not (eval f)
	| AndAlso(f1, f2) -> eval f1 && eval f2
	| OrElse(f1, f2) -> eval f1 || eval f2
	| Imply(f1, f2) -> not (eval f1) || eval f2
	| Equal(e1, e2) -> arithmatic e1 = arithmatic e2;;