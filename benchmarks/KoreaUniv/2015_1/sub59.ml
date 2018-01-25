(* Problem 1 *)
let rec factorial n =
  match n with
      0 -> 1
     |1 -> 1
     |_ -> n * factorial (n - 1);;

let pascal : int * int -> int
=fun (x,y) -> if x=0 || y=0 then 1
              else factorial x/(factorial (x - y) * factorial y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a = b then f a else f a + sigma f (a+1) b;;


(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
        [] -> 0
       |h::[] -> h
       |h1::t1 -> match t1 with
                [] -> h1
               |h::[] -> if h > h1 then h else h1
               |h2::t2 -> if h1 > h2 then max (h1::t2) else max (h2::t2);;

let rec min : int list -> int
=fun l -> match l with
        [] -> 0
       |h::[] -> h
       |h1::t1 -> match t1 with
                [] -> h1
               |h::[] -> if h < h1 then h else h1
               |h2::t2 -> if h1 < h2 then min (h1::t2) else min (h2::t2);;

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
      | Neg p -> if p=True then false else true
      | Or (p, q) -> if p=False && q=False then false else true
      | And (p, q) -> if p=True && q=True then true else false
      | Imply (p, q) -> if p=True && q=False then false else true
      | Equiv (p, q) -> if p=True && q=True then true
                        else if p=False && q=False then true
                        else false;;


(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
           ZERO -> n2
          |SUCC (n3) -> natadd n3 (SUCC (n2))

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
           ZERO -> ZERO
          |SUCC (n3) -> natadd n2 (natmul n3 n2)
