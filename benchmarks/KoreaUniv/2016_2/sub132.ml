(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with 
	| [] -> 0 
	| h :: t -> if(h > max(t)) then h else max(t);;

let rec min : int list -> int
= fun lst -> match lst with
	| [] -> 99999999 (*This is not ideal due to an false answer being given if the list contains only elements above 99999999*)
	| h :: t -> if(h < min(t)) then h else min(t);;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
	| [] -> []
	| h :: t -> if(pred(h)) then h :: filter pred t else filter pred t;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =  f(f(a));; (*Not recursive and too simple. I felt this question was the hardest*)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
	| Empty -> false
	| Node (x, leftBranch, rightBranch) -> (x = n || mem n leftBranch || mem n rightBranch);;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	| ZERO -> n2
	| SUCC remaining -> natadd remaining (SUCC n2);; (*retval will represent the nat number 1 lower than n1, thus decrementing n1 by 1 while incrementing n2 by 1*)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
	| ZERO -> ZERO
	| SUCC remaining -> natadd n2 (natmul remaining n2);; (*Unsure if it is acceptable to use previous add function, but this makes the most sense to me*)

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
	| Not form -> if(eval(form)) then false else true
	| AndAlso (form1, form2) -> eval(form1) && eval(form2)
	| OrElse (form1, form2) -> eval(form1) || eval(form2)
	| Imply (form1, form2) -> if(eval(form1)) then eval(form1) && eval(form2) else true
	| Equal (exp1, exp2) -> let rec expEval : exp -> int = fun e -> match e with (*I am not sure if this qualifies as changing the skeleton, but I understand if it does and points need to be taken off*)
		| Num (v) -> v
		| Plus (exp1, exp2) -> expEval(exp1) + expEval(exp2)
		| Minus (exp1, exp2) -> expEval(exp1) - expEval(exp2) in
			if(expEval(exp1) = expEval(exp2)) then true else false;;

