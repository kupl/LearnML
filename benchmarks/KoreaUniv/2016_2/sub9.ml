let rec fold f lst base
= match lst with
| [] -> base
| [i] -> i
| hd::tl -> f hd (fold f tl base)

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> fold (fun a b -> if a > b then a else b) lst 0

let rec min : int list -> int
= fun lst -> fold (fun a b -> if a < b then a else b) lst 0

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst
= match lst with
| [] -> []
| hd::tl -> if pred hd then hd::(filter pred tl) else (filter pred tl)

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
= fun n tree -> match tree with
| Empty -> false
| Node (i, left, right) -> n = i || mem n left || mem n right

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> n1
| SUCC n3 -> natadd (SUCC n1) n3

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> ZERO
| SUCC n3 -> natadd n1 (natmul n1 n3)

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
= fun f -> match f with
| True -> true
| False -> false
| Not f1 ->
	if eval f1 then false else true
| AndAlso (f1, f2) ->
	if eval f1 then eval f2 else false
| OrElse (f1, f2) ->
	if eval f1 then true else eval f2
| Imply (f1, f2) ->
	if eval f1 then eval f2 else true
| Equal (exp1, exp2) ->
	(let rec e2i exp =
		(match exp with
		| Num i -> i
		| Plus (e1, e2) -> e2i e1 + e2i e1
		| Minus (e1, e2) -> e2i e1 - e2i e2
		)
	in e2i exp1 = e2i exp2
	)
	

















