(* Problem 1*)
let rec pascal : int * int -> int 
=fun (x,y) -> 
if x=0 && y=0 then 1
else if y<0 || x<y then 0
else pascal(x-1, y-1) + pascal(x-1,y) (* TODO *)


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
if a=b then f a
else f a + sigma f (a+1) b

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 
	match l with
	|[] -> 0
  |x::[] -> x
	|h::t ->
		let v = max t in
		if h > v then h else v
 
let rec min : int list -> int
=fun l -> 
	match l with
	|[] -> 0
	|x::[] -> x
	|h::t ->
		let v = min t in
		if h < v then h else v

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
	| True -> true
	| False -> false
	| Neg(a) -> if a = True then false else true
	| Or(a,b) -> if (a=True)||(b=True) then true else false
	| And(a,b) -> if (a=True)&&(b=True) then true else false
	| Imply(a,b) -> if (a=True)&&(b=False) then false else true
	| Equiv(a,b) -> if a=b then true else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
let rec length n =
	match n with
  	ZERO -> 0
	| SUCC(a) -> 1 + length a
in let rec result m =
	if m=0 then ZERO
	else SUCC(result (m-1))
in result ((length n1)+(length n2))

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
let rec length n =
	match n with
  	ZERO -> 0
	| SUCC(a) -> 1 + length a
in let rec result m =
	if m=0 then ZERO
	else SUCC(result (m-1))
in result ((length n1)*(length n2))

