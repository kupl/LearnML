(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a)

let max : int list -> int 
= fun lst -> 
	(match lst with
	| [] -> raise (Failure "Empty list cannot have max value")
	| _ -> fold (fun x y -> if x >= y then x else y) lst min_int)

let min : int list -> int
= fun lst ->
	(match lst with
	| [] -> raise (Failure "Empty list cannot have min value")
	| _ -> fold (fun x y -> if x <= y then x else y) lst max_int)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	match lst with
	| [] -> []
	| h::t -> if pred h then h::(filter pred t) else filter pred t

(*********************)
(*     Problem 3     *)
(*********************)
let inc x = x + 1

let mul x = x * 2

let rec double f a = f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem a bt = 
	match bt with
	| Empty -> false
	| Node (i,b1,b2) ->
		if i = a then true
		else if mem a b1 then true
		else mem a b2   

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd n1 n2 = 
	match n2 with
	| ZERO -> n1
	| SUCC ZERO -> SUCC n1
	| SUCC t -> SUCC (natadd n1 t) 

let rec natmul n1 n2 = 
	match n2 with
	| ZERO -> ZERO
	| SUCC ZERO -> n1
	| SUCC t -> natadd (natmul n1 t) n1

let two = SUCC (SUCC ZERO)

let three = SUCC two

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

let rec cal e =
	match e with
	| Num i -> i
	| Plus (e1, e2) -> cal e1 + cal e2
	| Minus (e1, e2) -> cal e1 - cal e2

let rec eval f =
	match f with
	| True -> true
	| False -> false
	| Not t -> if eval t then false else true
	| AndAlso (f1, f2) -> eval f1 && eval f2
	| OrElse (f1, f2) -> eval f1 || eval f2
	| Imply (f1, f2) -> if eval f1 && eval f2 = false then false else true
	| Equal (e1, e2) -> cal e1 = cal e2	
