(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if x=y || y=0 then 1 else
pascal (x-1, y-1) + pascal (x-1, y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a <= b then
(f a) + sigma f (a+1) b else 0;;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
[] -> min_int
|h::t -> if max t > h then max t else h;;

let rec min : int list -> int
=fun l -> match l with
[] -> max_int
|h::t -> if min t < h then min t else h;;

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
|False -> false
|And (x, y) -> eval x && eval y
|Or (x, y) -> eval x || eval y
|Neg x -> if eval x then false else true
|Imply (x, y) -> if eval x = true && eval y = false then false else true
|Equiv (x, y) -> if eval x = eval y then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> n2
| SUCC(x) -> natadd x (SUCC(n2));;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> ZERO
| SUCC ZERO -> n2
| SUCC(x) -> natadd n2 (natmul x n2);;
