(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if y = 0 || x = y then 1 else pascal (x-1, y-1) + pascal (x-1, y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a!=b then f a + sigma f (a+1) b else f a;;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
  [] -> 0
  |[x] -> x
  |hd::tl -> if hd >= max tl then hd else max tl;;

let rec min : int list -> int
=fun l -> match l with
  [] -> 0
  |[x] -> x
  |hd::tl -> if hd <=  min tl then hd else min tl;;

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
  |True -> true 
  |False -> false
  |Neg x -> if x = True then false else true
  |Or (x, y) -> if x = False && y = False then false else true
  |And (x, y) -> if x = True && y = True then true else false
  |Imply (x, y) -> if x = True && y = False then false else true
  |Equiv (x, y) -> if x = y then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n2 with
  |ZERO -> n1
  |SUCC ZERO -> SUCC(n1)
  |SUCC(x) -> SUCC(natadd n1 x);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n2 with
  |ZERO -> ZERO
  |SUCC ZERO -> n1
  |SUCC(x) -> natadd (natmul n1 x) n1;;