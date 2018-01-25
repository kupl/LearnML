(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> List.fold_left (fun x y -> if y > x then y else x) (List.hd lst) lst

let rec min : int list -> int
= fun lst -> List.fold_left (fun x y -> if y < x then y else x) (List.hd lst) lst

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst
= List.fold_right (fun x a -> if pred x then x :: a else a) lst []

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a 
= f (f a)

(*********************)
(*     Problem 4     *)
(*********************)

type btree =
	| Empty
	| Node of int * btree * btree

let getLeftChild: btree -> btree
= fun tree -> match tree with 
|Empty -> raise(Failure "Tree does not have a left child.")
|Node (a,b,c) -> b

let getRightChild: btree -> btree
= fun tree -> match tree with 
|Empty -> raise(Failure "Tree does not have a right child.")
|Node (a,b,c) -> c

let getInt: btree -> int
= fun tree -> match tree with 
|Empty -> raise(Failure "Tree is empty.")
|Node (a,b,c) -> a

let rec mem : int -> btree -> bool
= fun n tree -> 
if tree = Empty then false 
else if n = getInt tree then true
else mem n (getLeftChild tree) || mem n (getRightChild tree)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with 
| ZERO -> n1
| SUCC(a) -> natadd (SUCC n1)  a

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match (n1, n2) with 
| (ZERO, b) -> ZERO
| (a, ZERO) -> ZERO
| (SUCC(ZERO), b) -> n2
| (a, SUCC(ZERO)) -> n1
| (a, SUCC(b)) -> natadd a (natmul a b)

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
	
	
let rec evalHelper1: exp -> int
= fun f -> match f with 
| Num (a) -> a 
| Minus (a,b) -> (evalHelper1 a) - (evalHelper1 b)
| Plus (a,b) -> (evalHelper1 a) + (evalHelper1 b)
	
let rec evalHelper: formula -> formula 
= fun f -> match f with
| True -> True
| False -> False
| Not(a) -> if a = True then False else True
| AndAlso(a,b) -> if (evalHelper a) = True && (evalHelper b) = True then True else False
| OrElse(a,b) -> if (evalHelper a) = False  && (evalHelper b) = False then False else True
| Imply(a,b) -> if (evalHelper a) = True && (evalHelper b) = False then False else True 
| Equal(a,b) -> if (evalHelper1 a) = (evalHelper1 b) then True else False


let rec eval : formula -> bool
= fun f -> match f with
| True -> true
| False -> false 
| f -> eval (evalHelper f)




