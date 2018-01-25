
(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if y = 0 then 1 else if x == y then 1 
else pascal (x-1, y-1) + pascal (x-1,y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a > b then 0 else f a + sigma f (a + 1) b

(* Problem 3 *)
let rec max : int list -> int
=fun list -> match list with
| [x] -> x
| h::t -> if h > (max t) then h else max t


let rec min : int list -> int
=fun list -> match list with
| [x] -> x
| h::t -> if h < (min t) then h else min t

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
=fun formula -> match formula with
| True -> true
| False -> false
| Neg x -> if x=True then false else true
| Or(x,y) -> if x=False && y=False then false else true
| And(x,y) -> if x=True && y=True then true else false
| Imply(x,y) -> if x=True && y=False then false else true
| Equiv(x,y) -> if x=y then true else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC n -> natadd n (SUCC n2)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC ZERO -> n2
| SUCC n -> natadd n2 (natmul n n2)