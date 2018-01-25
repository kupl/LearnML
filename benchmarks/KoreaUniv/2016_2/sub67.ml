(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
match lst with
|[] -> -2000000000
|hd::tl -> if(hd > (max tl)) then hd else max tl 

let rec min : int list -> int
= fun lst ->
match lst with
|[] -> 2000000000
|hd::tl -> if(hd < (min tl)) then hd else min tl

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
match lst with
|[] -> []
|hd::tl -> if(pred hd) then (hd::(filter pred tl)) else (filter pred tl)

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
| Node(a,b,c) -> if(a=n ||(mem n b)||(mem n c)) then true else false

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
match n1 with
|ZERO -> n2
|SUCC(a) -> natadd a (SUCC(n2))

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
match n1 with
|ZERO -> ZERO
|SUCC(a) -> natadd n2 (natmul a n2)


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

let rec calc : exp -> int
= fun e ->
match e with
|Num(a) -> a
|Plus(a,b) -> (calc (a) + calc (b))
|Minus(a,b) -> (calc(a) - calc(b))

let rec eval : formula -> bool
= fun f ->
match f with
| True -> true
| False -> false
| Not(a) -> if(eval(a)) then false else true
| AndAlso(a,b) -> if(eval(a)&&eval(b)) then true else false
| OrElse(a,b) -> if(eval(a)||eval(b)) then true else false
| Imply(a,b) -> if(eval(a)=false) then true else eval(b) 
| Equal(a,b) -> if((calc a)=(calc b)) then true else false



