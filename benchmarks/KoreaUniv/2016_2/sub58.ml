(*********************)
(*     Problem 1     *)
(*********************)

let rec max : int list -> int
= fun lst -> match lst with
| [] -> raise (Failure "The list is too short")
| [a] -> a
| a :: b :: tl ->
	if a < b then max (b :: tl)
	else max (a :: tl) 

let rec min : int list -> int
= fun lst -> match lst with
| [] -> raise (Failure "The list is too short")
| [a] -> a
| a :: b :: tl ->
	if a < b then min (a :: tl)
	else min (b :: tl) 

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
| [] -> []
| hd :: tl ->
	if pred hd then hd :: (filter pred tl)
	else (filter pred tl) 

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
| Node (a, b1, b2) ->
	if a = n then true
	else mem n b1 || mem n b2 

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> n1
| SUCC a -> SUCC (natadd n1 a)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> ZERO
| SUCC a -> natadd n1 (natmul n1 a)

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

let rec evalexp : exp -> int
= fun exp -> match exp with
| Num n -> n
| Plus (x, y) -> (evalexp x) + (evalexp y)
| Minus (x, y) -> (evalexp x) - (evalexp y)

let rec eval : formula -> bool
= fun f -> match f with
| True -> true
| False -> false
| Not a -> not (eval a)
| AndAlso (a, b) -> (eval a) && (eval b)
| OrElse (a, b) -> (eval a) || (eval b)
| Imply (a, b) -> (not (eval a)) || (eval b)
| Equal (x, y) -> (evalexp x) = (evalexp y)



