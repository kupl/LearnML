(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
match lst with
[] -> 0
|h::[] -> h
|h::t -> if h > max t then h else max t;;


let rec min : int list -> int
= fun lst -> 
match lst with
[] -> 0
|h::[] -> h
|h::t -> if h < min t then h else min t;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
match lst with
[] -> []
|h::t -> if pred h = true then h::filter pred t else filter pred t;;


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
Empty -> false
|Node (x, left, right) ->
if n = x then true else mem n left || mem n right;;



(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
match n2 with
ZERO -> n1
|SUCC(x) -> natadd (SUCC(n1)) x;;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
let rec loopsum : nat -> nat -> nat
= fun s x ->
match x with
ZERO -> s
|SUCC(x) -> loopsum (natadd s n1) x in
loopsum ZERO n2;;


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
let rec expEval : exp -> int
= fun e ->
match e with
Num(x) -> x
|Plus(a, b) -> expEval a + expEval b
|Minus(a, b) -> expEval a - expEval b in
match f with
True -> true
|False -> false
|Not(x) -> not (eval x)
|AndAlso(a, b) -> (eval a) && (eval b)
|OrElse(a, b) -> (eval a) || (eval b)
|Imply(a, b) -> (not (eval a)) || (eval b)
|Equal(a, b) -> if expEval a = expEval b then true else false;;

