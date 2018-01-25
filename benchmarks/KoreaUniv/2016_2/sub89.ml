(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
match lst with
| [] -> 0
| hd::tl -> ( if tl = [] then hd else ( if hd < (max tl) then ( max tl ) else hd ) )

let rec min : int list -> int
= fun lst -> 
match lst with
| [] -> 0
| hd::tl -> ( if tl = [] then hd else ( if hd > (min tl) then ( min tl ) else hd ) );;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
match lst with
| [] -> []
| hd::tl -> ( if (pred hd) then (hd::(filter pred tl)) else filter pred tl );;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);;

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
| Node (num,t1,t2) -> if num = n then true else ((mem n t1) || (mem n t2));;

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
| SUCC (n) -> SUCC (natadd n n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
match n1 with
| ZERO -> ZERO
| SUCC (n) -> natadd n2 (natmul n n2);;

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

let rec eval2 : exp -> int
= fun e ->
match e with
| Num (e1) -> e1
| Plus (e1,e2) -> (eval2 e1) + (eval2 e2)
| Minus (e1,e2) -> (eval2 e1) - (eval2 e2)

let rec eval : formula -> bool
= fun f -> 
match f with
| True -> true
| False -> false
| Not (formula) -> (if (eval formula) = true then false else true)
| AndAlso (formula1, formula2) -> (eval formula1 && eval formula2)
| OrElse (formula1, formula2) -> (eval formula1 || eval formula2)
| Imply (formula1, formula2) -> 
(if ((eval formula1)=true && (eval formula2)=false) then false else true)
| Equal (exp1, exp2) -> (if (eval2 exp1) = (eval2 exp2) then true else false);;
