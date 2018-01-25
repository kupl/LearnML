(*********************)
(*     Problem 1     *)
(*********************)
let max1 a b = if a>=b then a else b
let min1 a b = if a<=b then a else b
let rec max : int list -> int
= fun lst -> match lst with
| [hd] -> hd
| hd :: tl -> max1 hd (max tl)

let rec min : int list -> int
= fun lst -> match lst with
| [hd] -> hd
| hd :: tl -> min1 hd (min tl)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
|[] -> []
| hd :: tl -> if pred hd then hd :: filter pred tl
else filter pred tl

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f(f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with 
|Empty -> false 
|Node(a, bt1, bt2)-> if n = a then true
else if n<a then mem n bt1
else mem n bt2

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> let rec suc = fun k -> match k with
             | ZERO -> ZERO
             | SUCC a -> SUCC (suc a)
             in suc n1
| SUCC b -> SUCC(natadd n1 b )

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> ZERO
| SUCC a -> natadd n1 (natmul n1 a)
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
| Not n -> not (eval n)
| AndAlso(e1,e2) -> eval e1 && eval e2
| OrElse(e1,e2) -> eval e1 || eval e2
| Imply(e1,e2) -> if eval e1 then eval e2  else true
| Equal(e1,e2) -> let rec ex = fun n -> match n with
            | Num k -> k
            | Plus (v1,v2) ->ex v1 + ex v2
            | Minus (v1,v2) -> ex v1 - ex v2
            in if ex e1 = ex e2 then true else false
