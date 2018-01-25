(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1 if y = 0 || x= y
else pascal(x-1 ,y-1) + pascal (x-1,y);;

(* Problem 2 *)
let rec sigma f a b = fun in
if a= b -> b
else a + sigma f a+1 b;;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 1 (* TODO *)

let rec min : int list -> int
=fun l -> 1 (* TODO *)

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
=fun f -> true (* TODO *)

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO(* TODO *)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO (* TODO *)