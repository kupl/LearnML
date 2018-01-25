(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
if y=0 then 1
else if x=y then 1
else pascal(x-1,y-1)+pascal(x-1,y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
if b=1 then 1
else if a!=1 then (sigma f 1 b)-(sigma f 1 a)
else f b + sigma f a (b-1);;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with (* TODO *)
[] -> 0
|hd::tl -> if (tl=[]) then hd
else if (hd>=(max tl)) then hd
else max tl;;

let rec min : int list -> int
=fun l -> match l with (* TODO *)
[]-> 0
|hd::tl -> if (tl=[]) then hd
else if (hd<=(min tl)) then hd
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
True -> true
|False -> false
|Neg f -> eval f
|Or (f,g) -> if (eval f || eval g) then true else false
|And (f,g) -> if(eval f && eval g) then true else false
|Imply (f,g) -> if(eval f) then eval g else true
|Equiv (f,g) -> if(eval f == eval g) then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> n2
|SUCC n1' -> SUCC(natadd n1' n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> ZERO
|SUCC ZERO -> n2
|SUCC n1' -> SUCC(match n2 with
|ZERO -> ZERO
|SUCC ZERO -> SUCC ZERO
|SUCC n2' -> SUCC (natmul n1' (natmul n1 n2')));;

