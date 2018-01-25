(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if (y=0 || y=x) then 1
else pascal(x-1,y-1) + pascal(x-1,y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a=b then f a
else (f a) + (sigma f (a+1) b);;

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
| [a] -> a
| h::t -> let m = max t in
if(h>m) then h
else m;;

let rec min : int list -> int
=fun l -> match l with
| [a] -> a
| h::t -> let m = min t in
if(h<m) then h
else m;;

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
=fun f -> match f with True -> true
 | False -> false
 | Neg p when eval (p) = true -> false
 | Neg p when eval (p) = false -> true
 | Or (p,q) when eval (p) = true || eval (q) = true -> true
 | Or (p,q) when eval (p) = false && eval (q) = false -> false
 | And (p,q) when eval (p) = true && eval (q) = true -> true
 | And (p,q) when eval (p) = false || eval (q) = false -> false
 | Imply (p,q) when eval (p) = false && eval (q) = false -> true
 | Imply (p,q) when eval (p) = true && eval (q) = false -> false
 | Imply (p,q) when eval (q) = true -> true
 | Equiv (p,q) when eval (p) = eval (q) = true -> true
 | Equiv (p,q) when eval (p) = eval (q) = false -> false

(* Problem 5 *)
type nat = ZERO | SUCC of nat;;

let two = SUCC (SUCC ZERO);;
let three = SUCC ( SUCC (SUCC ZERO));;

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> (match n2 with
	| ZERO -> ZERO
	| _ -> let SUCC q = n2 in
		SUCC (natadd ZERO q))
| _ -> let SUCC p = n1 in
	SUCC (natadd p n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC ZERO ->
	(match n2 with
	| ZERO -> ZERO
	| _ -> let SUCC q = n2 in
		SUCC (natmul (SUCC ZERO) q))
| _ -> (match n2 with
	| ZERO -> ZERO
	| SUCC ZERO -> SUCC ZERO
	| _ -> let SUCC q = n2 in
		let SUCC p = n1 in
		SUCC (natmul p (natmul (SUCC ZERO) q)));;