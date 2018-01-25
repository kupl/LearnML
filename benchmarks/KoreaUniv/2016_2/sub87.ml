(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l=
	match l with
		| [] -> 0
		| [a] -> a
		| hd::tl -> if f hd (fold f tl) then hd else (fold f tl)

let rec max : int list -> int
= fun lst -> fold (>) lst

let rec min : int list -> int
= fun lst -> fold (<) lst

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst=
	match lst with
		| [] -> []
		| hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl

(*********************)
(*     Problem 3     *)
(*********************)

let mul x =
	x * 2
let inc x =
	x + 1
let rec double f a =
	(f (f a))



(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree
	
let t1 = Node (1, Empty, Empty)
let t2 = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty))

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
		| Empty -> false
		| Node (a, Empty, Empty) -> a=n
		| Node (a, b, c) -> if a=n then true else (mem n b || mem n c)


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
		| ZERO -> n1
		| SUCC (a) -> natadd (SUCC (n1)) a

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	if n1=ZERO || n2=ZERO then
		ZERO
	else
		match n2 with
			| SUCC (a) -> natadd (natmul n1 a) n1
			| ZERO -> ZERO

let two = SUCC (SUCC ZERO)
let three = SUCC (SUCC (SUCC ZERO))


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

let rec evalexp : exp -> int
= fun e ->
	match e with
		| Plus (a, b) -> evalexp a + evalexp b
		| Minus (a, b) -> evalexp a - evalexp b
		| Num a -> a

let rec eval : formula -> bool
= fun f ->
	match f with
		| True -> true
		| False -> false
		| Not (a) -> if eval a then false else true
		| AndAlso (a, b) -> if eval a&&eval b then true else false
		| OrElse (a, b) -> if eval a||eval b then true else false
		| Imply (a, b) -> if eval b||(eval a=eval b) then true else false
		| Equal (a, b) -> if evalexp a=evalexp b then true else false
