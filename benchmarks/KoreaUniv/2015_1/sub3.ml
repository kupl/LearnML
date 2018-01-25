(* Problem 1 *)
let rec pascal : int * int -> int
= fun(x, y) ->
if x < y then 0
else if x = 0 || y = 0 then 1
else pascal(x-1, y-1) + pascal(x-1, y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if a = b then f(a)
else f(a) + (sigma f (a+1) b);;

(* Problem 3 *)
let rec max : int list -> int      
= fun l ->
match l with
| [] -> min_int
| hd :: tl -> 
if hd > max tl then hd
else max tl;;

let rec min : int list -> int
=fun l ->
match l with
| [] -> max_int
| hd :: tl -> 
if hd < min tl then hd
else min tl;;

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
= fun p ->
match p with
| True -> true
| False -> false
| Neg value -> not (eval value)
| Or (left, right) -> (eval left) || (eval right)
| And (left, right) -> (eval left) && (eval right)
| Imply (left, right) -> not (eval left) || (eval right)
| Equiv (left, right) -> (eval left) = (eval right);;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
  = fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC value -> SUCC (natadd value n2);;

let rec natmul : nat -> nat -> nat
  = fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC value -> natadd n2 (natmul value n2);;

