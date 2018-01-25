(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if x = y || y = 0 then 1 else pascal(x - 1, y - 1) + pascal(x - 1, y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a = b then f a else f b + sigma f a (b - 1);;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
[] -> 0
|h::t -> if h > max t then h else max t;;

let rec min : int list -> int
=fun l -> match l with
[] -> 9999
|h::t -> if h < min t then h else min t;;

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
|False -> false
|Neg f2 -> not (eval f2)
|Or (f2, f3) -> eval f2 || eval f3
|And (f2, f3) -> eval f2 && eval f3
|Imply (f2, f3) -> if eval f2 then eval f3 else true
|Equiv (f2, f3) -> if eval f2 = eval f3 then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
match n1 with
ZERO -> n2
|SUCC(n1_minus) -> natadd n1_minus (SUCC(n2));;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
match n1 with
ZERO -> ZERO
|SUCC ZERO -> n2
|SUCC(n1_minus) -> natadd n2 (natmul n1_minus n2);;