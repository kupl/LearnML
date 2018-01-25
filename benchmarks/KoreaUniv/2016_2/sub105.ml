(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with |[]->0|hd::[]->hd |hd::tl -> if hd>max tl then hd else max tl;;

let rec min : int list -> int
= fun lst -> match lst with |[]->0|hd::[] ->hd | hd::tl -> if hd<min tl then hd else min tl;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with |[]->[] | hd::tl -> if pred hd then hd::(filter pred tl) else (filter pred tl);;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f(f(a));;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with |Empty->false |Node(x,t1,t2)-> if x=n then true else mem n t1 || mem n t2;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with |ZERO->n2 |SUCC(x)->natadd x (SUCC(n2));;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with |ZERO->ZERO |SUCC ZERO ->n2 |SUCC(x)->natadd (natmul x n2) n2;;

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
= fun f ->match f with |True->true|False->false |Not x -> if eval x = true then false else true | AndAlso(x,y)->if eval x = false then false else if eval y = false  then false else true | OrElse(x,y)->if eval x =true then true else if eval y = true  then true else false | Imply(x,y) -> if eval x = true&&eval y = false  then false else true | Equal(x,y)->let rec ev : exp->int = fun e -> match e with |Num n -> n |Plus(x,y) -> ev x + ev y |Minus(x,y)-> ev x - ev y in if ev x = ev y then true else false;;


