(* Problem 1 *)

let rec pascal : int * int -> int
= fun (x,y) -> (match x, y with
| _, 0 -> 1 
| _, _ -> if x = y then 1 else pascal (x-1,y-1) + pascal (x-1,y))


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a = b then f b else f a + sigma f (a+1) b


(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
| [] -> 0
| hd::tl -> if hd > (max tl) then hd else (max tl)

let rec min : int list -> int
=fun l -> match l with
| [] -> 0
| hd::tl -> if hd < (min tl) then hd else (min tl)


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
| Neg n -> if n = True then false else true
| Or (n1,n2) -> if n1 = False && n2 = False then false else true
| And (n1,n2) -> if n1 = True && n2 = True then true else false
| Imply (n1, n2) -> if n1 = True && n2 = False then false else true
| Equiv (n1, n2) -> if n1 = n2 then true else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat
(*  test value *)
let two = SUCC (SUCC ZERO)
let three = SUCC ( SUCC (SUCC ZERO) )

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1, n2 with
| _ , ZERO -> n1
| ZERO , _ -> n2
| SUCC n1 , _ -> SUCC (natadd n1 n2)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1, n2 with
| _ , ZERO -> ZERO
| ZERO , _ -> ZERO
| SUCC n1,_ -> natadd n2 (natmul n1 n2)