(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
	let rec fold action l a = 
		match l with
		| [] -> a
		| hd::tl -> action hd (fold action tl a) in
	let max a b = if a > b then a else b in
	fold max lst 0

let rec min : int list -> int
= fun lst -> 
	let rec fold action l a = 
		match l with
		| [] -> a
		| hd::tl -> action hd (fold action tl a) in
	let min a b = if a < b then a else b in
	fold min lst max_int

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	let rec aux i = function
      | [] -> []
      | h :: t -> if pred h then h :: aux (i+1) t else aux (i+1) t in
    aux 1 lst

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
	f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	let rec leaves = function
	    | Empty -> []
	    | Node(c, Empty, Empty) -> [c]
	    | Node(_, l, r) -> leaves l @ leaves r in
	let rec search a = function
		| [] -> false
		| hd::tl -> if hd = a then true else search a tl in
	search n (leaves tree)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	let rec add a = function
		| ZERO -> a
		| SUCC(nat) -> add (SUCC a) nat in
	if n1 = ZERO then add n2 n1 else add n1 n2

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	let rec mul n1 n2 = function
			| ZERO -> ZERO
			| SUCC(ZERO) -> n1
			| SUCC(nat) -> mul (natadd n1 n2) n2 nat in
	if n1 = ZERO then mul n2 n2 n1 else mul n1 n1 n2

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
	let rec tf = function
		| True -> true
		| False -> false
		| Not(f) -> if tf f then false else true 
		| AndAlso(a,b) -> if (tf a && tf b)  then true else false 
		| OrElse(a,b) -> if (tf a || tf b)  then true else false 
		| Imply(a,b) -> if((tf a = true) && (tf b = false)) then false else true 
		| Equal(a,b) -> 
			let rec calc = function
			|Num(a) -> a
			|Plus(a,b) -> calc a + calc b
			|Minus(a,b) -> calc a - calc b in 
			if (calc a = calc b) then true else false 
		in tf f

