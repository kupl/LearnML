(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a)

let rec max : int list -> int
= fun lst -> fold (fun x y -> if x>=y then x else y) lst min_int

let rec min : int list -> int
= fun lst -> fold (fun x y -> if x<=y then x else y) lst max_int

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
	match lst with
	| [] -> []
	| hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl
(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
	(f(f a))

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
	| Node(v, l, r) -> if n=v then true else mem n l || mem n r

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
	| SUCC(n) -> natadd n (SUCC(n2))

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	match n1 with
	| ZERO -> ZERO
	| SUCC(n) -> natadd n2 (natmul n n2)

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
	let rec number : exp -> int = fun e ->
	match e with
	| Num(n) -> n
	| Plus(e1, e2) -> number e1 + number e2
	| Minus(e1, e2) -> number e1 - number e2
	in
	match f with
	| True -> true
	| False -> false
	| Not(f1) -> not (eval f1)
	| AndAlso(f1,f2) -> eval f1&& eval f2
	| OrElse(f1, f2) -> eval f1|| eval f2
	| Imply(f1, f2) -> if eval f1 = true && eval f2 = false then false else true
	| Equal(e1,e2) -> if number e1 = number e2 then true else false




	


