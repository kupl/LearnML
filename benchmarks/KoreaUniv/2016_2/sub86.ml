exception Problem

let rec fold f l a =
	match l with
	|[] -> a
	|hd::tl -> f hd (fold f tl a)

let rec fold_base f l =
        match l with
        |[] -> raise Problem
	|hd::[] -> hd
        |hd::tl -> f hd (fold_base f tl)

(*********************)
(*     Problem 1     *)
(*********************)
let max : int list -> int = fun l -> 
	fold_base (fun x y -> if x > y then x else y) l

let min : int list -> int  = fun l ->
	fold_base (fun x y -> if x < y then x else y) l

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
	fold (fun x y -> if pred x then x::y else y) lst []

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
	|Empty -> false
	|Node(p,l,r) -> if p = n then true 
			else (mem n l) ||  (mem n r)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with 
		|ZERO -> n1
		|SUCC tl -> SUCC (natadd n1 tl)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
		|ZERO -> ZERO
		|SUCC ZERO -> n2
		|SUCC tl -> natadd n2 (natmul tl n2) 

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
	|True		-> true
	|False		-> false
	|Not x		-> if (eval x) = true then false else true
	|AndAlso (x,y)	-> 
		begin
		match eval x, eval y with
		|true,true -> true
		|_ -> false
		end
	|OrElse (x,y)	-> 
		begin
		match eval x, eval y with
		|false , false -> false
		|_ -> true
		end
	|Imply (x,y)	-> 
		begin
		match eval x, eval y with
		|true,false -> false
		|_ -> true
		end
	|Equal (x,y)	-> 
		let rec exp_eval e =
			begin
			match e with		
			|Num e1 -> e1	
			|Plus (e1,e2) -> (exp_eval e1) + (exp_eval e2)
			|Minus (e1,e2) -> (exp_eval e1) - (exp_eval e2)
			end
		in
		if exp_eval x = exp_eval y then true else false
