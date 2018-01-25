(* exception *)
exception Improper_input

(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
if x < 0 || y < 0 || x < y then raise Improper_input
else if y = 0 | x = y then 1
else pascal (x-1, y-1) + pascal (x-1,y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
if a > b then raise Improper_input
else if a = b then (f b)
else (f a) + (sigma f (a + 1) b)

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 
match l with
| [] -> raise Improper_input
| [hd] -> hd
| hd::tl -> if hd > (max tl) then hd else (max tl)

let rec min : int list -> int
=fun l -> 
match l with
| [] -> raise Improper_input
| [hd] -> hd
| hd::tl -> if hd < (min tl) then hd else (min tl)

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
True -> true
| False -> false
| Neg(x) -> not (eval x)
| Or(x,y) -> (eval x) || (eval y)
| And(x,y) -> (eval x) && (eval y)
| Imply(x,y) -> not (eval x) || (eval y)
| Equiv(x,y) -> (eval x) = (eval y)

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
match n2 with
| ZERO -> n1
| (SUCC n2) -> SUCC (natadd n1 n2)


let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
match n2 with
| ZERO -> ZERO
| (SUCC n2) -> natadd n1 (natmul n1 n2)

let one = SUCC ZERO
let two = SUCC (SUCC ZERO)
let three = SUCC (SUCC (SUCC ZERO))
let four = SUCC (SUCC (SUCC (SUCC ZERO)))