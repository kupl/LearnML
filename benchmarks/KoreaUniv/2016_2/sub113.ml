(*********************)
(*     Problem 1     *)
(*********************)
exception There_Is_No_Element

let rec fold1 f l =
	match l with
	| [] -> raise There_Is_No_Element
	| hd::[] -> hd
	| hd::tl -> f hd (fold1 f tl)

let rec max : int list -> int
= fun lst -> fold1 (fun x y -> if x > y then x else y) lst

let rec min : int list -> int
= fun lst -> fold1 (fun x y -> if x < y then x else y) lst

(*********************)
(*     Problem 2     *)
(*********************)
let rec fold2 f l =
	match l with
	| [] -> []
	| hd::tl -> f hd (fold2 f tl)

let rec filter pred lst = fold2 (fun x y -> (if pred x then [x] else []) @ y) lst

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
	| Empty -> false
	| Node (a,t1,t2) -> if a = n then true else (mem n t1) || (mem n t2)

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
	| SUCC (n2_inside) -> natadd (SUCC (n1)) n2_inside

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
	| ZERO -> ZERO
	| SUCC (n2_inside) -> natadd n1 (natmul n1 n2_inside)

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

let rec cal : exp -> int
= fun e ->
	match e with
	| Num (n) -> n
	| Plus (e1,e2) -> (cal e1) + (cal e2)
	| Minus (e1,e2) -> (cal e1) - (cal e2)

let rec eval : formula -> bool
= fun f ->
	match f with
	| True -> true
	| False -> false
	| Not (f1) -> not(eval(f1))
	| AndAlso (f1,f2) -> eval(f1) && eval(f2)
	| OrElse (f1,f2) -> eval(f1) || eval(f2)
	| Imply (f1,f2) -> eval(Not (f1)) && eval(f2)
	| Equal (e1,e2) ->
		let rec cal : exp -> int
		= fun e ->
			match e with
			| Num (n) -> n
			| Plus (x,y) -> (cal x) + (cal y)
			| Minus (x,y) -> (cal x) - (cal y)
		in ((cal e1) = (cal e2))
