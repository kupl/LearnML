(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if (x=0)||(x=y)||(y=0) then 1
 else pascal ((x-1),(y-1)) + pascal ((x-1),(y))

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a > b then 0 else f a + sigma f (a+1) b

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
| [] -> raise (Failure "error")
| [a] -> a
| hd::tl -> if hd > (max tl) then hd else max tl

let rec min : int list -> int
=fun l -> match l with
| [] -> raise (Failure "error")
| [a] -> a
| hd::tl -> if hd < (min tl) then hd else min tl

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
| Neg a -> if a=True then false else true
| Or(a,b) -> if a=False && b=False then false else true
| And(a,b) -> if a=True && b=True then true else false
| Imply(a,b) -> if a=True && b=False then false else true
| Equiv(a,b) -> if a=b then true else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC n3 -> natadd n3 (SUCC n2)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC ZERO -> n2
| SUCC n3 -> natadd n2 (natmul n3 n2)