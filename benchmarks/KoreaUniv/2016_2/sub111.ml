(*********************)
(*     Problem 1     *)
(*********************)
let big a b = if a > b then a else b

let rec max : int list -> int
= fun lst ->
match lst with
|[]->(-10000000000)
|hd::tl->big hd (max tl);;

let small a b = if a < b then a else b

let rec min : int list -> int
= fun lst ->
match lst with 
|[]->10000000000
|hd::tl-> small hd (min tl)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
match lst with
|[]->[]
|hd::tl->
if (pred hd) = true then
[hd]@(filter pred tl)
else
filter pred tl

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
| Node (x,t1,t2) -> n = x || mem n t1 || mem n t2

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat (*nat is Set of  SUCC ZERO, ZERO, SUCC (SUCC ZERO) *)

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
match n1 with
| ZERO -> n2
| SUCC n -> SUCC (natadd n n2)

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
match n1 with
| ZERO -> ZERO
| SUCC n -> natadd (natmul n n2) n2

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

let rec solver e =
match e with
|Num n -> n
|Plus(e1,e2)->(solver e1)+(solver e2)
|Minus(e1,e2)->(solver e1)-(solver e2)

let rec eval : formula -> bool
= fun f -> 
match f with
|True -> true
|False -> false
|Not(f1) -> not(eval f1)
|Imply (f1,f2)->not(eval f1) || (eval f2)
|Equal (e1,e2)-> (solver e1) = (solver e2)
|AndAlso (f1,f2)->(eval f1) && (eval f2)
|OrElse (f1,f2)->(eval f1) || (eval f2)

let t1 = Node (1,Empty,Empty)
let t2 = Node (1, Node(2,Empty,Empty),Node(3,Empty,Empty))

