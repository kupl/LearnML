(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
	|[] -> 0
	|hd::tl -> List.fold_left (fun x y -> if x > y then x else y) hd lst;; 

let rec min : int list -> int
= fun lst -> match lst with
	|[] -> 0
	|hd::tl -> List.fold_left (fun x y -> if x > y then y else x) 1000000 lst;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
	match lst with
	| [] -> []
	| hd::tl -> if (pred hd) then hd::(filter pred tl)
							else (filter pred tl);;

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
= fun n tree -> match tree with
	| Empty -> false
	| Node (nn, left, right) -> n = nn || mem n left || mem n right;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
	| ZERO -> (n2)
	| SUCC (nn1) -> SUCC(natadd nn1 n2);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with 
	| ZERO -> ZERO
	| SUCC (nn1) -> (natadd (natmul nn1 n2) n2);;
   

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
	| Not (nf) -> not(eval (nf))
	| AndAlso (nf1, nf2) -> (eval (nf1)) && (eval (nf2))
	| OrElse (nf1, nf2) -> (eval (nf1)) || (eval (nf2))
	| Imply (nf1, nf2) -> not(eval (nf1)) || (eval (nf2))
	| Equal (nf1, nf2) -> let rec calnum = fun f1 -> match f1 with
	| Num n -> n
	| Plus (n1, n2) -> (calnum (n1)) + (calnum (n2))
	| Minus (n1, n2) -> (calnum (n1)) - (calnum (n2))
	in (calnum (nf1)) = (calnum (nf2));;
 
