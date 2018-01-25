(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec  mirror : btree -> btree
= fun t -> match t with
 | Empty -> Empty
 | Node ( x, Empty, Empty ) -> Node ( x, Empty, Empty )
 | Node ( x, t1, t2 ) -> Node ( x, mirror t2, mirror t1) 



(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
 | ZERO -> n2
 | SUCC n1 -> SUCC (natadd n1 n2)
 
let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
 | ZERO -> ZERO
 | SUCC ZERO -> n2
 | SUCC n1 -> natadd n2 ( natmul n1 n2 )

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
 | ZERO -> ZERO
 | SUCC ZERO -> SUCC ZERO
 |_ -> if n2 = ZERO then SUCC ZERO else SUCC ZERO 



(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec sat : formula -> bool
= fun f -> match f with
  | True -> true
  | False -> false
  | Neg f -> not (sat f)
  | And (f1, f2) -> (sat f1)&&(sat f2)
  | Or (f1, f2) -> (sat f1) || (sat f2)
  | Imply (f1, f2) -> if (sat f1) = false then true else (sat f2)
  | Iff (f1, f2) -> if (sat Imply f1, f2) = true && (sat Imply f2, f1) = true then true else false



