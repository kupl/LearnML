(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
[] -> -100000
| hd::tl -> if hd > max(tl) then hd else max(tl);;  

let rec min : int list -> int
= fun lst -> match lst with
[] -> 100000 
| hd::tl -> if hd <min(tl) then hd else min(tl);; 

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with [] -> []
| hd::tl -> if (pred hd) then hd :: (filter pred tl) 
else filter pred tl;;
 

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f(f a);;
 

(*********************)
(*     Problem 4     *)
(*********************)
let first p = match p with (x,_,_) -> x;;
let second p = match p with (_,x,_) -> x;;
let third p = match p with (_,_,x) -> x;;

type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> true;; 
 
 

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC m -> natadd m (SUCC n2);;
 

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> ZERO
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
= fun f -> match f with
True -> true 
| False -> false
| Not (a) -> if a = True then false else true
| OrElse (a, b) -> if (a = True || b = True) then true else false
| AndAlso (a,b) -> if ( a = True && b = True) then true else false
| Imply (a,b) -> if (a = True && b = False) then false else true 
| Equal (a,b) -> if (a = b) then true else false;;

 

