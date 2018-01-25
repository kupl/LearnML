(*********************)
(*     Problem 1     *)
(*********************)
exception Problem

let rec fold f l =
match l with
|[]->raise Problem
|[i]->i
|hd::tl -> f hd (fold f tl);;

let rec max : int list -> int
 = fun lst -> fold (fun a b -> if a>b then a else b) lst ;;

let rec min : int list -> int
 = fun lst ->fold (fun a b -> if a>b then b else a) lst ;;
(*********************)
(*     Problem 2     *)
(*********************)

let rec filter pred lst =

match lst with
|[] -> []
|hd::tl -> if pred hd == true  then hd::(filter pred tl) else filter pred tl;;
(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f ( f a );;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem n tree = 
match tree with
|Empty -> false
|Node (x,l,r) -> if n=x then true
 else (mem n l)||(mem n r);;


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd n1 n2 = 
match n1  with
|ZERO -> n2
|SUCC n1 -> SUCC (natadd n1 n2);; 

let rec natmul n1 n2 =
match n1 with
|ZERO -> ZERO
|SUCC n1 ->natadd (natmul n1 n2) n2;;


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



let rec eval_op op =
 match op with
|Num a -> a
|Minus(exp1, exp2) -> (eval_op exp1) - (eval_op exp2)
|Plus(exp1, exp2) -> (eval_op exp1) + (eval_op exp2) 

and eval f =
match f with
|True -> true
|False -> false
|Not f -> not (eval f)
|AndAlso (f1,f2)-> (eval f1)&&(eval f2)
|OrElse (f1,f2)-> (eval f1)||(eval f2)
|Imply (f1,f2) -> (not(eval f1))&&(eval f2)
|Equal (exp1,exp2) ->  if ((eval_op exp1) = (eval_op exp2)) then true else false;;

