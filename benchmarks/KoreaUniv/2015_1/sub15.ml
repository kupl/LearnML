(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
	if x=0 || y = 0 then 1
	else if x==y then 1
	else pascal(x-1, y-1) + pascal(x-1, y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
	if b = a then a
	else
		f b + sigma f a (b-1)


(* Problem 3 *)
let rec max : int list -> int
=fun l ->
	match l with
	| [] -> raise (Failure "Emplty list!")
	| x::[] -> x
	| x::y -> 
		let temp = max y in
		if x > temp then x else temp

let rec min : int list -> int
=fun l ->
	match l with
	| [] -> raise (Failure "Emplty list!")
	| x::[] -> x
	| x::y -> 
		let temp = min y in
		if x < temp then x else temp

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
	| Neg x -> eval x
	| Or (x, y) -> if eval x || eval y then true else false
	| And (x, y) -> if eval x && eval y then true else false
	| Imply (x, y) -> if (eval x && eval y==false) then false else true
	| Equiv (x, y) -> if eval x = eval y then true else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
	match n1, n2 with
	| SUCC x, SUCC y -> SUCC( SUCC( natadd x y )) 
	| SUCC x, ZERO -> SUCC( natadd x ZERO )
	| ZERO, SUCC y -> SUCC( natadd ZERO y )
	| ZERO, ZERO -> ZERO

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
	match n1, n2 with
	| SUCC x, SUCC y -> natadd (natmul x n2) n2
	| SUCC x, ZERO -> ZERO
	| ZERO, SUCC y -> ZERO
	| ZERO, ZERO -> ZERO
