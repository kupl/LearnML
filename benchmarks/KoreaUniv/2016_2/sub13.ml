(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
[a] -> a
| hd::tl -> if hd > max tl then hd else max tl 
| [] -> 0

let rec min : int list -> int
= fun lst -> match lst with
[a] -> a
| hd::tl -> if hd < min tl then hd else min tl
| [] -> 0

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
match lst with
[] -> [] | hd::tl -> if (pred hd) then hd::(filter pred tl)
else filter pred tl

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =
f (f (a))

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
Empty -> false |
Node (k, l, m) -> if k == n then true
else (if mem n l then true
else (if mem n m then true else false))

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
SUCC( k ) -> natadd (SUCC (n1)) k 
| ZERO -> n1
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
SUCC( k ) -> natadd n1 (natmul n1 k)
| ZERO -> ZERO

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

let rec eval : formula -> bool =
let rec expfunc =
fun e -> match e with
Num k -> k|
Plus (k, l) -> (expfunc k) + (expfunc l)|
Minus (k, l) -> (expfunc k) - (expfunc l)

in fun f -> match f with
Not k -> if (eval k) == true then false else true |
AndAlso (k, l) -> if(eval k == true  && eval l == true) then true else false| 
OrElse (k, l) -> if(eval k == true || eval l == true) then true else false|
Imply (k, l) -> if(eval k == true && eval l == false) then false else true|
Equal (k, l) -> if (expfunc k == expfunc l) then true else false|
True -> true | False -> false
