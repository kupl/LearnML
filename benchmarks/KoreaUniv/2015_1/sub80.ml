(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) ->  if y=0 then if x=0 then 1 else pascal(x-1,0) else if y=x then pascal(x-1,y-1) else pascal(x-1,y-1) + pascal(x-1,y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a<b then f a+ sigma f (a+1) b else f b

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with 
[] -> -1
| h::[] -> h
| h::t -> let temp = max t in if h>temp then h else temp

let rec min : int list -> int
=fun l -> match l with 
[] -> -1
| h::[] -> h
 | h::t -> let temp = min t in if h<temp then h else temp

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
| Neg (x) -> if x=True then false else true
| Or (a,b) -> if (a=True) || (b=True) then true else false
| And (a,b) -> if (a=True && b=True) then true else false
| Imply (a,b) -> if a=True then if b=True then true else false else true
| Equiv (a,b) -> a=b

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> n2
| SUCC(a) -> SUCC(natadd a n2)


let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with 
ZERO -> ZERO
| SUCC(a) -> natadd (natmul a n2) n2