let rec fold f l a =
	match l with
	| [] -> a
	| h::t -> f h (fold f t a)

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> fold (fun x y -> if x > y then x else y) lst min_int

let rec min : int list -> int
= fun lst -> fold (fun x y -> if x < y then x else y) lst max_int

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
	| [] -> []
	| h::t -> if pred h then h::(filter pred t) else filter pred t

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
	| Node (i, t1, t2) -> if i = n then true
										 else (mem n t1) || (mem n t2)
	| Empty -> false

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	match n2 with
	| SUCC a -> SUCC (natadd n1 a)
	| ZERO -> n1

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	match n2 with
	| SUCC a -> if a = ZERO then n1 else natadd n1 (natmul n1 a)
	| ZERO -> ZERO

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

let rec sub_fun : exp -> int
= fun e ->
	match e with
	| Num i -> i
	| Plus (e1, e2) -> (sub_fun e1) + (sub_fun e2)
	| Minus (e1, e2) -> (sub_fun e1) - (sub_fun e2)

let rec eval : formula -> bool
= fun f ->
	match f with
	| True -> true
	| False -> false
	| Not a -> if eval a then false else true
	| AndAlso (a1, a2) -> if eval a1 && eval a2 then true else false
	| OrElse (a1, a2) -> if eval a1 || eval a2 then true else false
	| Imply (a1, a2) -> if eval a1 && eval(Not a2) then false else true
	| Equal (e1, e2) -> if (sub_fun e1) = (sub_fun e2) then true else false


