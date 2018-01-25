
(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
| [] -> raise (Failure "The list is empty.")
| hd::[] -> hd
| hd::tl -> let m = max(tl) in
if (hd < m) then m
else hd

let rec min : int list -> int
= fun lst -> match lst with
| [] -> raise (Failure "The list is empty.")
| hd::[] -> hd
| hd::tl -> let m = min(tl) in
if (hd < m) then hd
else m

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
match lst with
| [] -> []
| hd::tl -> let l = [] in 
if pred(hd) = true then l @ [hd] @ (filter pred tl)
else l @ (filter pred tl)
   
(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f(f(a))

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
| Empty -> false
| Node(a, left, right) -> if a = n then true
else if mem n left = true then true
else if mem n right = true then true
else false

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC n_1 -> natadd n_1 (SUCC n2)  

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC n_1 ->  natadd n2 (natmul n_1 n2) 

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
| Not ev -> if ev = True then false else true
| AndAlso (ev1, ev2) -> if ev1 = True && ev2 = True then true else false 
| OrElse (ev1, ev2) -> if ev1 = False && ev2 = False then false else true
| Imply (ev1, ev2) -> if ev1 = True && ev2 = False then false else true
| Equal (in1, in2) -> let rec pl_mi x =
	match x with
		| Num (n) -> n
		| Plus (n1, n2) -> pl_mi(n1) + pl_mi(n2)
		| Minus (n1, n2) -> pl_mi(n1) - pl_mi(n2)
	in
	if pl_mi(in1) = pl_mi(in2) then true else false
