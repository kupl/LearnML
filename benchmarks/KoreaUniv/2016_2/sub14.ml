(*********************)
(*     Problem 1     *)
(*********************)
exception Invalid_input

let rec fold1 f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold1 f tl a)

let rec max : int list -> int
= fun lst ->  (* TODO *)
	match lst with
	| [] -> raise Invalid_input
	| hd::tl -> fold1 (fun x y -> if x>y then x else y) lst min_int

let rec min : int list -> int
= fun lst ->  (* TODO *)
	match lst with
	| [] -> raise Invalid_input
	| hd:: tl -> fold1 (fun x y -> if x<y then x else y) lst max_int

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =  (* TODO *)
	match lst with
	| [] -> []
	| hd::tl -> 
		if pred hd then hd::(filter pred tl)
		else filter pred tl


(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =  (* TODO *)
	f (f a)


(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->  (* TODO *)
	match tree with
	| Empty -> false
	| Node (rt, ltree, rtree) ->
		if rt == n then true
		else if mem n ltree = true then true
		else if mem n rtree = true then true
		else false


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
	match n2 with
	| ZERO -> n1
	| SUCC (n) -> SUCC (natadd n1 n)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
	match n1,n2 with
	| ZERO,_ -> ZERO
	| _,ZERO -> ZERO
	| _,SUCC(ZERO) -> n1
	| SUCC(ZERO),_ -> n2
	| _,SUCC(n) -> natadd (natmul n1 n) n1 


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

let rec exp_eval : exp -> int
= fun e ->
	match e with
	| Num(n) -> n
	| Plus(n1,n2) -> exp_eval n1 + exp_eval n2
	| Minus(n1,n2) -> exp_eval n1 - exp_eval n2

let rec eval : formula -> bool
= fun f -> (* TODO *)
	match f with
	| True -> true
	| False -> false
	| Not(f1) -> if eval f1 = true then false else true
	| AndAlso(f1,f2) -> if eval f1 = true && eval f2 = true then true else false
	| OrElse(f1,f2) -> if eval f1 = false && eval f2 = false then false else true
	| Imply(f1,f2) ->
		if eval f1 = false then true
		else if eval f1 = true && eval f2 = true then true
		else false
	| Equal(exp1,exp2) ->
		if exp_eval exp1 = exp_eval exp2 then true
		else false


