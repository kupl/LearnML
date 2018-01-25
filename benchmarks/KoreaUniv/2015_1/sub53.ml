(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if (y = 0 || x = y) then 1 else pascal (x - 1, y - 1) + pascal (x - 1, y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if (a = b) then f a else f a + sigma f (a + 1) b;;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
|[x] -> x
|h::t -> if h > max t then h else max t;;

let rec min : int list -> int
=fun l -> match l with
|[x] -> x
|h::t -> if h < min t then h else min t ;;

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
	| Neg b -> eval b = false
	| Or (b1, b2) -> eval b1 || eval b2
	| And (b1, b2) -> eval b1 && eval b2
	| Imply (b1, b2) -> eval (Neg b1) || eval b2
	| Equiv (b1, b2) -> eval b1 = eval b2;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC(n) -> natadd n (SUCC n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC(n) -> natadd n2 (natmul n n2);;