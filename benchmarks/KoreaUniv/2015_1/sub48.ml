(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) ->
	if y = 0 then 1
	else if x=y then 1
	else pascal(x-1,y-1) + pascal(x-1,y)





(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
	if a = b then f b
	else if a > b then 0
	else sigma f a (b-1) + f b






(* Problem 3 *) 
let rec max : int list -> int
=fun l ->
	match l with
 	[] -> 0
	| head :: [] -> head
	| head :: tail ->
		if head > max tail then head
		else max tail

let rec min : int list -> int
=fun l -> 
	match l with
 	[] -> 0
	| head :: [] -> head
	| head :: tail -> 
		if head < min tail then head
		else min tail






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
	| Neg a -> not eval a
	| Or a b -> eval a || eval b
	| And a b -> eval a && eval b
	| Imply a b -> not eval a || eval b
	| Equiv a b -> eval a = eval b






(* Problem 5 *)
type nat = ZERO | SUCC of nat
let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
	match n1, n2 with
	| ZERO, ZERO -> ZERO
	| SUCC (n1'), ZERO -> SUCC (natadd n1' n2)
	| ZERO, SUCC (n2') -> SUCC (natadd n1 n2')
	| SUCC (n1'), SUCC (n2') -> SUCC (natadd n1' n2)
	

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
	match n1, n2 with
	| ZERO, ZERO -> ZERO
	| SUCC (n1'), ZERO -> SUCC (natmul n1' n2)
	| ZERO, SUCC (n2') -> SUCC (natmul n1 n2')
	| SUCC (n1'), SUCC (n2') -> SUCC (natmul n1' n2)