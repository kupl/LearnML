let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a);;

(*     Problem 1     *)
let rec max : int list -> int
= fun lst -> 
	fold (fun x y -> if x > y then x else y) lst min_int;;

let rec min : int list -> int
= fun lst ->
	fold (fun x y -> if x < y then x else y) lst max_int;;

(*     Problem 2     *)
let rec filter pred lst = 
	match lst with
	| [] -> []
	| hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl;;

(*     Problem 3     *)
let rec double f a = 
	f (f a);;

(*     Problem 4     *)
type btree =
	| Empty
	| Node of int * btree * btree;;

let rec mem : int -> btree -> bool
= fun n tree -> 
	match tree with
	| Empty -> false
	| Node(x, node1, node2) ->
		if x = n then true else (mem n node1) || (mem n node2);;

(*     Problem 5     *)
type nat =
	| ZERO
	| SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
	| ZERO -> n1
	| SUCC(a) -> natadd (SUCC(n1)) a;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	match n2 with
	| ZERO -> ZERO
	| SUCC(a) -> natadd n1 (natmul n1 a);;

(*     Problem 6     *)
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
	let rec convert exp =
		match exp with
		| Num(a) -> a
		| Plus(a,b) -> (convert a) + (convert b)
		| Minus(a,b) -> (convert a) - (convert b)
			in
	match f with
	| True -> true
	| False -> false
	| Not(a) -> not (eval a)
	| AndAlso(a,b) -> (eval a) && (eval b)
	| OrElse(a,b) -> (eval a) || (eval b)
	| Imply(a,b) ->	(eval b) || not(eval a)
	| Equal(a,b) -> (convert a) = (convert b);;