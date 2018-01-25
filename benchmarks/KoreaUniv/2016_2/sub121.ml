(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
	let m a b = if a >= b then a else b
	in match lst with 
		[] -> 0
		|h::t -> List.fold_left (m) h t

let rec min : int list -> int
= fun lst -> 
	let m2 a b = if a <= b then a else b
	in match lst with 
		[] -> 0
		|h::t -> List.fold_left (m2) h t

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	let rec check l r = 
		match l with
		[] -> r
		|h::t -> if pred h then check t r@h else check t r
	in check lst []

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
	let temp = f a in f temp

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
	Empty -> false
	| Node (x, tree1, tree2) ->
	if x = n then true else mem n tree1 || mem n tree2

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	let rec s p q =
		match p with 
		ZERO -> q
		| SUCC (r) -> s r (SUCC q)
	in s n1 n2

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	let rec mm p q =
		match p with 
		SUCC ZERO -> q
		| SUCC (r) -> mm r (natadd q q)
	in if n1 = ZERO || n2 = ZERO then ZERO else mm n1 n2

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
	let rec ma2 e =
		match e with
		| Num (x) -> x 
		| Plus (x, y) -> (ma2 x) + (ma2 y)
		| Minus (x, y) -> (ma2 x) - (ma2 y)		
	in let rec ma p = 
		match p with 
		| True -> true
		| False -> false
		| Not(x) -> not(ma x)
		| AndAlso (x,y) -> (ma x) && (ma y)
		| OrElse (x,y) -> (ma x) || (ma y)
		| Imply (x,y) -> if ma x = true && ma y = false then false else true
		| Equal (x,y) -> if (ma2 x) = (ma2 y) then true else false
	in ma f