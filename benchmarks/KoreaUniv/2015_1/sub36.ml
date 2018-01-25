(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> match (x,y) with
| (_,1) -> 1
| (n,k) -> if n=k then 1 else pascal (n-1,k-1) + pascal (n-1, k);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a=b then f b else f b + (sigma (f) (a) (b-1));;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
| [n] -> n
| hd::tl -> if hd > max tl then hd
else max tl;;

let rec min : int list -> int
=fun l -> match l with
| [n] -> n
| hd::tl -> if hd < min tl then hd
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
=fun f -> match f with
| True -> true
| False -> false
| Neg a -> if eval a=true then false else true
| Or (c,d) -> if eval c=false && eval d=false then false else true
| And (e,f) -> if eval e=true && eval f=true then true else false
| Imply (g,h) -> if eval g=true && eval h=false then false else true
| Equiv (i,j) -> if eval i=eval j then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
match n1 with
| ZERO -> n2
| SUCC m1 -> natadd m1 (SUCC n2) 

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
match n1 with
| ZERO -> ZERO
| SUCC ZERO  -> n2
| SUCC(SUCC m2 ) -> natadd (natmul (SUCC m2)  n2) n2;;
