(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1 

let rec pascal (a,b) =
	if a = 0 then 1
	else if b = 0 then 1
	else if a = b then 1
	else (pascal ((a-1),(b-1)))  + (pascal ((a-1), b)) ;;


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 1 

let rec sigma test a b =
	if a = b then (test a)
	else (test b) + (sigma test a (b-1));;


(* Problem 3 *)
let rec max : int list -> int
=fun l -> 1 

let rec max l =
	match l with
 [] -> 0
 |h::t -> if h > (max t) then h else (max t);;


let rec min : int list -> int
=fun l -> 1 

let rec min l=
	match l with
	[] -> 100000000
 | h :: t -> if h < (min t) then h else (min t);;


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
=fun f -> true 

let rec eval s =
	match s with
	True -> true
	|False -> false
	|Neg f -> not (eval f)
	|Or (f, ff) ->(eval f) || (eval ff)
  |And (f, ff) ->(eval f) && (eval ff)
  |Imply (f , ff) -> not (not (eval f)  && (not (eval ff)))
	|Equiv (f , ff) -> (eval f)=(eval ff);;


(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO
let rec natadd n1 n2= 
	match n1 with
	ZERO -> n2
	|SUCC(h) -> (natadd h (SUCC n2)) ;;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO 
let rec natmul n1 n2=
	match n1 with
  | ZERO -> SUCC n2
	|SUCC(h) -> (natmul h (SUCC n2));;
