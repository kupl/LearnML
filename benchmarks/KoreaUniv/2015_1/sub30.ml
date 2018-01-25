(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if x = y || y = 0 then 1
else pascal (x - 1, y - 1) + pascal (x - 1, y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a = b then f a
else f a +sigma f (a + 1) b;;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
| [] -> failwith "Null"
| [hd] -> hd
| hd::tl -> if hd > max tl then hd else max tl;;

let rec min : int list -> int
=fun l -> match l with
| [] -> failwith "Null"
| [hd] -> hd
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
| True -> true
| False -> false
| Neg a -> not (eval a)
| Or (a, b) -> (eval a) || (eval b)
| And (a, b) -> (eval a) && (eval b)
| Imply (a, b) -> (not (eval a)) || ( eval b)
| Equiv (a, b) -> (eval a) == (eval b);;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC n1 -> SUCC (natadd n1 n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC n1 -> natadd n2 (natmul n1 n2);;
