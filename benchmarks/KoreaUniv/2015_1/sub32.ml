(* 2012210066 컴퓨터학과  조현상*)

(* problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1
let rec pascal (x,y) =
	if x=y then 1 else if y=0 then 1 else if y>x then raise(Failure "Error")
	else pascal (x-1,y-1) + pascal (x-1,y)

(* problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 1

let rec sigma f a b =
if a>b then 0
else if a=b then f b
else f b + sigma f a (b-1)

(* problem 3 *)
let rec max : int list -> int
=fun l -> 1

let rec max l =
 match l with
| [x]-> x
| [] -> raise (Failure "Error")
| hd::tl -> if hd > max tl then hd else max tl

let rec min : int list -> int
=fun l -> 1

let rec min l =
 match l with
| [x]-> x
| [] -> raise (Failure "Error")
| hd::tl -> if hd < min tl then hd else min tl

(* problem 4 *)
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

let rec eval logic =
 match logic with
|True -> true
|False -> false
|Neg (fst) -> not (eval (fst))
|Or (fst, snd) -> ((eval (fst)) || (eval (snd)))
|And (fst, snd) -> ((eval (fst)) && (eval (snd)))
|Imply (fst, snd) -> 
	 (if (eval (fst) = false) then true
	  else if (eval (snd) = true) then true
	  else false
   )
|Equiv (fst, snd) ->
	 (if (eval (fst) = eval(snd)) then true
	  else false
   )

(* problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO

let rec natadd (a, b)  =
	match (a, b) with
 	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC (xval)) -> a
  | (SUCC (xval), ZERO) -> b
 	| (SUCC (yval), SUCC (zval)) -> SUCC( SUCC( natadd (yval, zval)))

let rec natmul (c, d) =
 	match (c, d) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC (eval)) -> ZERO
	| (SUCC (eval), ZERO) -> ZERO
	| (SUCC (fval), SUCC (gval)) -> natadd( natmul (fval, d), d)

