(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
if x=y then 1
else if y=0 then 1
else pascal(x-1,y-1)+pascal(x-1,y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
if a!=b then f b + sigma f a (b-1)
else f a;;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 
	match l with
	| []-> raise(Failure "List is too short!")
	| [a] -> a
	| hd::tl ->
		if hd < max tl then max tl
		else hd;;

let rec min : int list -> int
=fun l -> 
	match l with
	| []-> raise(Failure "List is too short!")
	| [a] -> a
	| hd::tl ->
		if hd > min tl then min tl
		else hd;;
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
=fun f ->
match f with
|True -> true
|False -> false
|Neg(x) -> not(eval(x))
|And(x,y) -> eval(x)&&eval(y)
|Or(x,y) -> eval(x)||eval(y)
|Imply(x,y) -> not(eval(x))||eval(y)
|Equiv(x,y) -> if eval(x) = eval(y) then eval(True)
	else eval(False)
;;


(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
match n1 with
|ZERO -> n2
|SUCC x -> SUCC(natadd n2 x);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
match n1 with
|ZERO -> ZERO
|SUCC x -> natadd n2 (natmul n2 x);;
