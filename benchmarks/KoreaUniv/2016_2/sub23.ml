(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> List.fold_left(fun a b -> if a>b then a else b)
						(List.hd lst) lst;;

let rec min : int list -> int
= fun lst -> List.fold_left(fun a b -> if a<b then a else b)
						(List.hd lst) lst;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
	begin match lst with
		| [] -> []
		| hd :: rest -> if (pred hd) then hd :: (filter pred rest)
									  else filter pred rest
	end;;

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
= fun n tree -> match tree with Empty -> false
	|Node (x, left, right) ->
		if n=x then true else
		if n<x then mem n left else mem n right;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	|ZERO -> n2
	|SUCC ZERO -> SUCC n2
	|SUCC (SUCC n) -> SUCC(SUCC(natadd n n2));;
		
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
	|ZERO -> ZERO
	|SUCC ZERO -> n2
	|SUCC n -> natadd n2 (natmul n n2);;

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
	match f with
	|True -> true
	|False -> false
	|Not (a) -> not (eval a)
	|AndAlso (a,b) -> eval (a) && eval (b)
	|OrElse (a,b) -> eval (a) || eval (b)
	|Imply (a,b) -> if eval (a) then eval (b) else true
	|Equal (a,b) ->
		let rec _eval x =
			match x with
			|Num (n) -> n
			|Plus (n1, n2) -> _eval (n1) + _eval (n2)
			|Minus (n1, n2) -> _eval (n1) - _eval (n2)
		in
	if _eval (a) < _eval (b) then true else false;;
