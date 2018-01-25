(*********************)
(*     Problem 1     *)
(*********************)
(* choose_one's input list should be non-empty *)
let rec choose_one f lst =
	let rec loop e l =
		match l with
		|	[] -> e
		|	h :: t -> loop (f e h) t in
	match lst with [] -> 0 | h :: t -> loop h t

let rec max : int list -> int
= fun lst -> choose_one (fun a b -> if a > b then a else b) lst

let rec min : int list -> int
= fun lst -> choose_one (fun a b -> if a < b then a else b) lst

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	let rec loop i o =
		match i with
		|	[] -> o
		|	h :: t -> loop t (if pred h then h :: o else o) in
	let rec reverse i o =
		match i with
		|	[] -> o
		|	h :: t -> reverse t (h :: o) in
	reverse (loop lst []) []

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
	|	Empty -> false
	|	Node (v, l, r) -> (n = v) || (mem n l) || (mem n r)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	match n1 with
	|	ZERO -> n2
	|	SUCC t -> natadd t (SUCC n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	let rec loop a r =
		match a with
		|	ZERO -> r
		|	SUCC t -> loop t (natadd n2 r) in
	loop n1 ZERO

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
	match f with
	|	True -> true
	|	False -> false
	|	Not p -> not (eval p)
	|	AndAlso (p, q) -> (eval p) && (eval q)
	|	OrElse (p, q) -> (eval p) || (eval q)
	|	Imply (p, q) -> if eval p then eval q else true
	|	Equal (a, b) -> 
			let rec calc e =
				match e with
				|	Num i -> i
				|	Plus (i, j) -> (calc i) + (calc j)
				|	Minus (i, j) -> (calc i) - (calc j) in
			(calc a) = (calc b)