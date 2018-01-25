(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if y=0 || y=x then 1 else pascal(x-1,y-1) + pascal(x-1,y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if b<=a then f(a) else f(b) + sigma f a (b-1);;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
[] -> min_int
| hd::tl -> if hd > max tl then hd else max tl;;

let rec min : int list -> int
=fun l -> match l with
[] -> max_int
| hd::tl -> if hd < min tl then hd else min tl;;

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
| Neg x -> if x=True then false else true
| Or(x,y) -> eval x || eval y
| And(x,y) -> eval x && eval y
| Imply(x,y) -> if x=True && y=False then false else true
| Equiv(x,y) -> if eval x = eval y then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> n2
| SUCC(n1) -> natadd n1 (SUCC(n2));;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> ZERO
| SUCC ZERO -> n2
| SUCC(n1) -> natadd n2 (natmul n1 n2);;
