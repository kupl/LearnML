(*********************)
(*     Problem 1     *)
(*********************)
let bigger x y =
if x > y then x
else y ;;
let rec fold bigger l a =
match l with
| [] -> a
| hd::tl -> bigger hd (fold bigger tl a) ;;
let rec max : int list -> int
= fun lst -> fold bigger lst 0 ;;

let smaller x y =
if x > y then y
else x ;;
let rec fold smaller l a =
match l with
| [] -> a
| hd::tl -> smaller hd (fold smaller tl a) ;;
let rec last l =
match l with
| [a] -> a
| _::tl -> last tl ;;
let rec min : int list -> int
= fun lst -> fold smaller lst (last lst) ;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
match lst with
| [] -> []
| hd::tl -> if (pred hd) then hd::(filter pred tl)
else (filter pred tl) ;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f(f a) ;;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
| Empty -> false
| Node (a, b, c) -> if a = n then true
else (mem n b)||(mem n c) ;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC(a) -> natadd a (SUCC n2) ;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC ZERO -> n2
| SUCC(a) -> natadd n2 (natmul a n2) ;;

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
| Not (True) -> false
| Not (False) -> true
| AndAlso (a, b) -> (eval a)&&(eval b)
| OrElse (a, b) -> (eval a)||(eval b) ;;

