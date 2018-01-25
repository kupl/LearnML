(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with (* TODO *)
| [] -> 0
| x::[] -> x
| hd::tl when hd>(max tl) -> hd
| hd::tl ->(max tl) 

let rec min : int list -> int
= fun lst -> match lst with (* TODO *)
| [] -> 0
| x::[] -> x
| hd::tl when hd<(min tl) -> hd
| hd::tl -> (min tl)
(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =  (* TODO *)
match lst with
| [] -> []
| hd::tl when (pred hd) = true -> hd::(filter pred tl)
| hd::tl -> (filter pred tl) 
(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =  (* TODO *)
f(f a)
(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->  (* TODO *)
match tree with
|Node (a, b, c) when n=a -> true
|Node (a, b, c) -> mem n b || mem n c
|Empty -> false
(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)
match n1 with
|ZERO -> n2
|SUCC a -> SUCC (natadd a n2) 

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)
match n2 with
|ZERO -> ZERO
|SUCC a -> natadd n1 (natmul n1 a)
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
= fun f -> (* TODO *)
match f with 
|	True -> true
| False -> false
| Not a when eval(a)=true -> false
| Not a -> true
| AndAlso (a,b) when eval(a) && eval(b) -> true
| AndAlso (a,b) -> false
| OrElse (a,b) when eval(a) = false && eval(b) = false -> false
| OrElse (a,b) -> true
| Imply (a,b) when eval(a) && eval(b)= false -> false
| Imply (a,b) -> true
| Equal (a,b) when a=b -> true
| Equal (a,b) ->false
