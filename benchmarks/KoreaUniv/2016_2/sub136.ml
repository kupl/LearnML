let rec fold f l a = match l with | [] -> a
	| hd::tl -> f hd (fold f tl a);;

let rec map f l = match l with | [] -> []
	| hd::tl -> (f hd)::(map f tl);;

let rec max : int list -> int
	= fun lst -> fold (fun x y -> if (x>y) then x else y) lst min_int;;

let rec min : int list -> int
	= fun lst -> 
		fold (fun x y -> if (x<y) then x else y) lst max_int;;

let rec filter pred lst =
	match lst with
		| [] -> []
		| hd::tl ->
			if (pred hd) then hd::(filter pred tl) else filter pred tl;;

let rec double f a = f (f a);;

type btree = | Empty | Node of int * btree * btree
(*
let t1 = Node (1, Empty, Empty)
let t2 = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty))
let t3 = Empty
*)
let rec mem : int -> btree -> bool
	= fun n tree -> match tree with
		| Empty -> false
		| Node (int,btreex,btreey) -> if int = n then true
			else if btreex=Empty then mem n btreey
			else if btreey=Empty then mem n btreex
			else mem n btreex || mem n btreey;;

type nat = | ZERO | SUCC of nat
(*
let two = SUCC (SUCC ZERO)
let three = SUCC (SUCC (SUCC ZERO))
*)
let rec natadd : nat -> nat -> nat
	= fun n1 n2 ->
		match n1 with
			| ZERO -> n2
			| SUCC n -> natadd n (SUCC n2);;

let rec natmul : nat -> nat -> nat
	= fun n1 n2 ->
		match n1 with
			| ZERO -> (*if n2=ZERO then ZERO
								else n2*)ZERO
			| SUCC ZERO -> n2
			| SUCC n -> natadd n2 (natmul n n2);;


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
			| True -> true
			| False -> false
			| Not x -> not (eval x)
									(*else raise (Failure "Type error : condition must be Bool type")*)
			| AndAlso (f1,f2) -> eval f1 && eval f2
			| OrElse (f1,f2) -> eval f1 || eval f2
			(*| Equal (f1,f2) -> if (f1=True && f2=True) then true
												else if (f1=False && f2=False) then true
												else false*)
			| Imply (f1,f2) -> not (eval f1) || eval f2
											 (*if (f1=True && f2=True) then eval True
											else if (f1=True && f2=False) then eval True
											else if (f1=False && f2=True) then eval False
											else if (f1=False && f2=False) then eval False
											else raise (Failure "Type Error : condition must be Bool type");;*)
			| Equal (f1,f2) -> let rec parseInt = fun x -> match x with
				| Num a -> a
				| Plus (f1,f2) -> (parseInt f1) + (parseInt f2)
				| Minus (f1,f2) -> (parseInt f1) - (parseInt f2) in (parseInt f1)=(parseInt f2);;




