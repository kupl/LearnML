(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if y=0 then 1
else if x=y then 1
else pascal(x-1,y-1)+pascal(x-1,y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if b=a then f a
else f b + sigma f a (b-1)


(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
[] -> 0
| h::t -> if h> max t then h
else if (max t)=0&&(t=[]) then h
else max t

let rec min : int list -> int
=fun l -> match l with
[] -> 0
| h::t -> if h< min t then h
else if (min t)=0&&(t=[]) then h
else min t

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
|Neg f -> if (eval f)=true then false
          else true
|Or (a,b) -> if (eval a) = true then true
             else if (eval b) = true then true
             else false
|And (a,b) -> if (eval a) = false then false
              else if (eval b) = false then false
              else true
|Imply (a,b) -> if (eval a) = false then true
                else if (eval b) = true then true
                else false
|Equiv (a,b) -> if (eval a)=(eval b) then true
                else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> n2
| SUCC x -> SUCC(natadd x n2)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> (match n2 with
	     ZERO -> ZERO
	     |SUCC y -> (match n1 with
			ZERO->ZERO
			|SUCC x -> natadd n1 (natmul n1 y)))