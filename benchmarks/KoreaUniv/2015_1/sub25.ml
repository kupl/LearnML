(* Problem 1 *) 
let rec pascal : int * int -> int
=fun (x,y) -> match (x,y) with
							| (0, 0) -> 1
							| (a, 0) -> 1
							| (a, b) -> if a = b then 1
													else pascal((a-1), (b-1)) + pascal((a-1), b)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a <> b then f a + sigma f (a+1) b
							else f b

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
					| [] -> 0
					| x :: [] -> x
					| x :: tail -> 
						 let v = max tail in
						 if x > v then x
						 else v

let rec min : int list -> int
=fun l -> match l with
				| [] -> 0
				| x :: [] -> x
				| x :: tail ->
					 let v = min tail in
					 if x < v then x
					 else v

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
=fun f -> match f with
				  | True -> true
					| False -> false
					| Neg v -> not (eval v)
					| Or (x,y) -> eval x || eval y
					| And (x,y) -> eval x && eval y
					| Imply (x,y) -> if eval x = true && eval y = false then false
													 else true
					| Equiv (x,y) -> if eval x = eval y then true
													 else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->	match n1 with
							| ZERO -> n2
							| SUCC x -> natadd x (SUCC n2)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
							| ZERO -> ZERO
							| SUCC x -> match n2 with
													| ZERO -> ZERO
													| SUCC y -> natadd n2 (natmul x n2)
