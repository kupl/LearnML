(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
if (x<0)||(y<0)||(x<y) then 0
else if (x<=1)||(y=0) then 1
else pascal(x-1,y-1) + pascal(x-1,y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
if a > b then 0
else sigma f a (b-1) + f b

(* Problem 3 *)
let rec max : int list -> int
=fun l ->
	match l with
	|[] -> 0
	|h::t -> if h > max t then h else max t 

let rec min : int list -> int
=fun l ->
	match l with
	|[] -> 2147483647
	|h::t -> if h < min t then h else min t

(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f ->
	match f with
	| True -> true
	| False -> false
	| Neg x-> if eval x then false else true
	| Or (x, y) -> if (eval x=true)||(eval y=true) then true else false
	| And (x, y) -> if (eval x=false)||(eval y=false) then false else true
	| Imply (x, y) ->
		(match eval x with
			|true -> eval y
			|false -> true)
	|Equiv (x, y) -> if (x=y) then true else false
	
(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
	match n1 with
	| SUCC a -> natadd a (SUCC n2)
	|_ -> n2


let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
	match n1 with
	|ZERO -> ZERO
	|SUCC a -> natadd n2 (natmul a n2)

	
