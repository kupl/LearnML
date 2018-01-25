(*********************)
(*  functions needed *)
(*********************)
let rec fold f l a =
	match l with
	| [] ->	a
	| hd::tl ->	f hd (fold f tl a)

let rec nth l n =
	match l with
	| [] ->	raise (Failure "List is shorter than expected.")
	| hd::tl ->	if n <= 0 then hd else nth tl (n - 1)

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
	fold (fun x y -> if x > y then x else y) lst (nth lst 0)

let rec min : int list -> int
= fun lst ->
	fold (fun x y -> if x < y then x else y) lst (nth lst 0)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
	| [] ->	[]
	| hd::tl ->	if pred hd then hd::(filter pred tl) else (filter pred tl) 

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
	match tree with
	| Empty ->	false
	| Node (n1, _, _) when n1 = n ->	true
	| Node (_, ltree, rtree) -> (mem n ltree) || (mem n rtree)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	match n1 with
	| ZERO ->	n2
	| SUCC (nn) ->	SUCC (natadd nn n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	match n1 with
	| ZERO ->	ZERO
	| SUCC (nn) ->	natadd n2 (natmul nn n2)

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

let rec eval_exp : exp -> int
= fun exp ->
	match exp with
	| Num n ->	n
	| Plus (n1, n2) ->	(eval_exp (n1)) + (eval_exp (n2))
	| Minus (n1, n2) ->	(eval_exp (n1)) - (eval_exp (n2))

let rec eval : formula -> bool
= fun f -> 
	match f with
	| True ->	true
	| False ->	false
	| Not (f1) ->	not (eval f1)
	| AndAlso (f1, f2) ->	(eval f1) && (eval f2)
	| OrElse (f1, f2) ->	(eval f1) || (eval f2)
	| Imply (f1, f2) ->
		begin
		match (eval f1, eval f2) with
		| (true, false) ->	false
		| _ ->	true
		end
	| Equal (e1, e2) ->
		if (eval_exp e1) = (eval_exp e2) then true else false
