

(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
	match l with
	|[] -> a
	|[n] -> n
	|hd::tl -> f hd (fold f tl a)

let max l = fold (fun x y -> if x>y then x else y) l 0
let min l = fold (fun x y -> if x<y then x else y) l 0


(*********************)
(*     Problem 2     *)
(*********************)
let rec filter f l =
	match l with
	|[]->[]
	|hd::tl -> if f hd then [hd] @ (filter f tl)
						else (filter f tl)
	
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

let rec mem x tree = 
	match tree with
	| Empty -> false
	| Node (k, l, r) -> if x = k then true
											else (mem x r) || (mem x l)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd n1 n2 =
	match n2 with
	| ZERO -> n1
	| SUCC (a) -> SUCC(natadd n1  a)

let rec natmul n1 n2 =
	match n2 with
	| ZERO -> ZERO
	| SUCC (a) -> natadd n1 (natmul n1 a)

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

let rec cal exp =
	match exp with
	| Num (e1) -> e1
	| Plus (e1, e2) -> let a = cal e1 + cal e2 in a
	| Minus (e1, e2) -> let a = cal e1 - cal e2 in a

let rec eval f =
	match f with
	| True -> true
 	| False -> false
	| Not (f1) -> if eval f1 then false else true
	| AndAlso (f1, f2) -> if eval f1 && eval f2 then true else false 
	| OrElse (f1, f2) -> if eval f1 || eval f2 then true else false
	| Imply (f1, f2) -> if eval f1 && eval (Not (f2)) then false else true
	| Equal (n1, n2) -> if (cal n1) = (cal n2) then true else false

