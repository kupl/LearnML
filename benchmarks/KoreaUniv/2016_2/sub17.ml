(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
	let greater a b = if a>b then a else b in
	match lst with
	hd::tl -> List.fold_left greater hd tl;;
	
let rec min : int list -> int
= fun lst -> 
	let smaller a b = if a>b then b else a in
	match lst with
	hd::tl -> List.fold_left smaller hd tl;;
(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
	|[]->[]
	|hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =
	f (f a);;

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
| Node (parent, left, right) ->
    parent = n || mem n left || mem n right;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
match n1 with
    ZERO -> n2
  | SUCC m -> natadd m (SUCC n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    ZERO -> ZERO
  | SUCC m -> natadd n2 (natmul m n2);; 

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
	let rec operate exp = 
		match exp with 
			|Num(a) -> a 
			|Plus(a, b) -> operate(a) + operate(b) 
			|Minus(a, b)-> operate(a) - operate(b) in
    match f with
    |True   -> true
    |False  -> false
    |Not(a) -> not (eval(a))
    |AndAlso (a,b) -> eval(a) && eval(b)
    |OrElse  (a,b) -> eval(a) || eval(b)
    |Imply   (a,b) -> not (eval(a)) || eval(b)
    |Equal   (a,b) -> operate(a)=operate(b);;