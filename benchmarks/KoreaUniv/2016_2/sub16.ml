(*********************)
(*     Problem 1     *)
(*********************)

let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a)

let rec max : int list -> int
= fun lst -> (* TODO *)
	fold (fun a b -> if(a>b) then a else b) lst min_int

let rec min : int list -> int
= fun lst -> (* TODO *)
	fold (fun a b -> if(b>a) then a else b) lst max_int

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = (* TODO *)
	match lst with
	| [] -> []
	| hd::tl -> if pred hd then hd::(filter pred tl)
							else filter pred tl
(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = (* TODO *)
	f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> (* TODO *)
	match tree with
	| Empty -> false
	| Node (a, left, right) ->
		if a = n then true
		else mem n left || mem n right	


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
	match n1 with
	| ZERO -> n2
	| SUCC(a) ->
		if a= ZERO then SUCC(n2)
		else SUCC (natadd a n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
	match n1 with
	| ZERO -> ZERO
	| SUCC(a) ->
		if a =ZERO then n2
		else natmul a ( natadd n2 n2 )
				

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

let rec exptoint : exp -> int
= fun e ->
	match e with
	| Num(a) -> a
	| Plus(a,b) -> exptoint a + exptoint b
	| Minus(a,b) -> exptoint a - exptoint b

let rec eval : formula -> bool
= fun f ->  (* TODO *)
	match f with
	| True -> true
	| False -> false
 	| Not(a) -> 
			if eval a = true then false
			else true
	| AndAlso(a,b) ->
			if eval a = true && eval b = true then true
			else false
	| OrElse(a,b) ->
			if eval a = true || eval b = true then true
			else false
	| Imply(a,b) ->
			if eval a = true && eval b = false then false
			else true
	| Equal(a,b) ->
			if exptoint a = exptoint b then true
			else false
