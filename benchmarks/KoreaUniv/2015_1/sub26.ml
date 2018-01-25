(* Problem 1 *)
let rec pascal : int * int -> int
= fun (x,y) ->
if(y=0) then 1
else if(x=0) then 0
else pascal (x-1,y-1) + pascal (x-1,y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if(a=b) then f a
else f a +sigma f (a+1) b

(* Problem 3 *)
let rec max : int list -> int
= fun l -> match l with
[]-> 0
|h::t -> if(t=[]) then h 
else if(h>max t) then h else max t


let rec min : int list -> int
= fun l -> match l with
[]->0
|h::t -> if(t=[]) then h 
else if(h>min t) then min t else h

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
= fun f -> match f with
  True-> true
| False-> false
| Neg for1 -> if(eval for1) then false else true
| Or(for1,for2) -> if(eval for1) then true else if(eval for2) then true else false
| And(for1,for2) -> if(eval for1=false) then false else if(eval for2=false) then false else true
| Imply(for1,for2) -> if(eval for1=false) then true else if(eval for2) then true else false
| Equiv(for1,for2) -> if(eval for1=eval for2) then true else false
(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> n2
|SUCC(n) -> SUCC(natadd n n2) 


let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> ZERO
|SUCC(n) -> natadd (natmul n n2) n2