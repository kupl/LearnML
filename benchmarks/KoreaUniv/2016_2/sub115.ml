let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd ( fold f tl a );;

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
	fold ( fun x y -> if x>y then x else y ) lst min_int;;

let rec min : int list -> int
= fun lst -> 
	fold ( fun x y -> if x<y then x else y ) lst max_int;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
match lst with
| [] -> []
| hd::tl -> 
	if pred hd then (hd)::( filter pred tl )
	else filter pred tl;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
	f ( f a );;

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
	| Node ( m, tree_L, tree_R ) -> 
		if n = m then true
		else ( mem n tree_L ) || ( mem n tree_R );;

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
	| SUCC n11 -> SUCC ( natadd n11 n2 );;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	match n1 with
	| ZERO -> ZERO
	| SUCC n11 -> natadd ( natmul n11 n2 ) n2;;
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

(*let rec exp : int -> int
= fun n ->
	match n with
	| Num k -> k
	| Plus (k, l) -> exp(k) + exp(l)
	| Minus (k, l) -> exp(k) - exp(l);;*)

let rec eval : formula -> bool
= fun f -> 
	match f with 
	| True -> true
	| False -> false
	| Not p -> not ( eval (p) )
	| AndAlso (p, q) -> eval (p) && eval (q)
	| OrElse (p, q) -> eval (p) || eval (q)
	| Imply (p, q) -> not ( eval (p) ) || eval(q)
	| Equal (p, q) -> p = q ;;
		(*if exp(p) = exp(q) then true
		else false;;*)




