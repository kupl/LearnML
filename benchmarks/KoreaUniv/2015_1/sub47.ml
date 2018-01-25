(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
if y = 0 || x = y then 1
else pascal (x-1, y-1) + pascal (x-1, y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
if f a = f b then f a
else f a + sigma f (a+1) b

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
| [] -> 0
| [x] -> x
| h :: t -> let mx = max t in
	if h > mx then h
	else mx
	
let rec min : int list -> int
=fun l -> match l with
| [] -> 0
| [x] -> x
| h :: t -> let mn = min t in
	if h < mn then h
	else mn

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
True -> true
| False -> false
| Neg a -> not (eval a)
| Or (a, b) -> eval a || eval b
| And (a, b) -> eval a && eval b
| Imply (a, b) -> not (eval a) || eval b
| Equiv (a, b) -> eval a = eval b

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> n2
| SUCC(n_minus_1) -> SUCC (natadd n_minus_1 n2)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> ZERO
| SUCC(n_minus_1) -> natadd n2 (natmul n_minus_1 n2)




let two = SUCC (SUCC ZERO)
let three = SUCC (SUCC (SUCC ZERO))
