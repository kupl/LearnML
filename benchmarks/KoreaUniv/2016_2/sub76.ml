(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
	let listHead : int list -> int
	= fun lst ->
		match lst with
		n::_ -> n
	|	[] -> (-1)
	in
	let rec findMax : int list -> int -> int
	= fun lst max ->
		match lst with
		n::t -> if max < n then findMax t n else findMax t max
	|	[] -> max
	in
	findMax lst (listHead lst)
	(*
	match max with
	List.fold_left (fun a b -> if a > b then a else b) max lst
	*)
let rec min : int list -> int
= fun lst ->
	let min = List.nth lst 0 in
	List.fold_left (fun a b -> if a < b then a else b) min lst

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
	[] -> []
|	n::lst2 -> if pred n then n :: filter pred lst2 else filter pred lst2

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
= fun n tree -> 
match tree with
	Node (num, t1, t2) -> if n = num then true else if (mem n t1) || (mem n t2) then true else false
|	Empty -> false

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	match n1, n2 with
	ZERO,ZERO -> ZERO
|	SUCC nat1,ZERO -> n1
|	ZERO,SUCC nat2 -> n2
|	SUCC nat1 ,SUCC nat2  -> natadd (SUCC n1) nat2

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	match n1, n2 with
	ZERO,_ -> ZERO
|	_,ZERO -> ZERO
|	SUCC nat1 ,SUCC nat2 ->
		if nat2 = ZERO then
			n1
		else
			natadd (natmul n1 nat2) n1

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
	True -> true
|	False -> false
|	Not f -> not (eval f)
|	AndAlso (f1, f2) -> eval f1 && eval f2
| OrElse (f1, f2) -> eval f1 || eval f2
|	Imply (f1, f2) -> if eval f1 = true && eval f2 = false then false else true
|	Equal (e1, e2) ->
		let rec evalNum : exp -> int
		= fun ex ->
		match ex with
			Num n -> n
		|	Plus (n1, n2) -> evalNum n1 + evalNum n2
		| Minus (n1, n2) -> evalNum n1 - evalNum n2
		in
		if evalNum e1 = evalNum e2 then true else false
