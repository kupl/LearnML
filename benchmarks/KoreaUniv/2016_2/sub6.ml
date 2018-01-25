(*********************)
(*     Problem 1     *)
(*********************)
exception Empty_list

let rec fold f l a =
match l with 
|[] -> a
|hd::tl -> f hd (fold f tl a)

let rec fold1 f l = 
match l with 
|[] -> raise Empty_list
|hd::tl -> fold f tl hd

let rec max : int list -> int
= fun lst -> fold1 (fun x y -> if x > y then x else y) lst 

let rec min : int list -> int
= fun lst -> fold1 (fun x y -> if x < y then x else y) lst 

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = fold (fun x y -> if pred x then x::y
else y) lst []

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
| Empty -> false
| Node(y, l, r) ->
	if n = y then true
	else (mem n l) || (mem n r)
 
(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> let rec evalnat n = 
					    match n with
							| ZERO -> ZERO
							| SUCC (n3) -> SUCC (evalnat n3)
in evalnat n1
| SUCC (b2) -> SUCC (natadd n1 b2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> ZERO
| SUCC (b2) -> natadd n1 (natmul n1 b2)

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
| Not x -> not (eval x)
| AndAlso (x, y) -> eval x && eval y
| OrElse (x, y) -> eval x || eval y
| Imply (x, y) -> if eval x then eval y else true
| Equal (x, y) -> let rec eval1 g = 
	match g with
	| Num n -> n
	| Plus (g1, g2) -> eval1 g1 + eval1 g2
	| Minus	(g1, g2) -> eval1 g1 - eval1 g2					
in if eval1 x = eval1 y then true else false 	
