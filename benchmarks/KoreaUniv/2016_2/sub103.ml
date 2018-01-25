(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a=
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a)

let rec max : int list -> int
= fun lst -> fold (fun x y -> if x>=y then x else y) lst min_int
let rec min : int list -> int
= fun lst -> fold (fun x y -> if x>=y then y else x) lst max_int

(*********************)
(*     Problem 2     *)
(*********************)
let rec addToListMap f l =
	match l with
	| [] -> []
	| hd::tl -> if(f hd) then hd::addToListMap f tl
							else addToListMap f tl 

let rec filter pred lst = addToListMap pred lst
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
									| Node(int, btree1, btree2) -> if int = n then true
																									else mem n btree1 || mem n btree2

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n1, n2 with
	|	SUCC(nat1), SUCC(nat2) -> SUCC(SUCC(natadd nat1 nat2))
	| ZERO, SUCC(nat2) -> SUCC(natadd ZERO nat2)
	| SUCC(nat1), ZERO -> SUCC(natadd nat1 ZERO)
	| ZERO, ZERO -> ZERO

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	match n1, n2 with
	| ZERO, _ -> ZERO
	| _, ZERO -> ZERO
	| SUCC(nat1), SUCC(nat2) -> natadd n1 (natmul n1 nat2) 

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

let rec expToInt : exp -> int
= fun exp1 ->
	match exp1 with
		| Num(int1) -> int1
		| Plus(e1, e2) -> expToInt e1 + expToInt e2
		| Minus(e1, e2) -> expToInt e1 - expToInt e2

let rec eval : formula -> bool
= fun f -> 
	match f with
	| True -> true
	| False -> false
	| Not(formula) -> if eval formula = true then false else true
	| AndAlso(formula1, formula2) -> 
			if eval formula1 = true && eval formula2 = true then true else false
	| OrElse(formula1, formula2) ->
			if eval formula1 = false && eval formula2 = false then false else true
	| Imply(formula1, formula2) ->
			if eval formula1 = false then true
			else if eval formula2 = true then true
			else false
	| Equal(exp1, exp2) -> if expToInt exp1 = expToInt exp2 then true else false
