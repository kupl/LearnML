(* Problem 1*)
let rec pascal : int * int -> int
=fun (x,y) ->
	if y=0 then 1
	else if x=y then 1
	else pascal (x-1, y-1) + pascal (x-1, y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
	if a>b then 0
	else ((f a) + sigma f (a+1) b)

(* Problem 3 *)
let rec max : int list -> int
=fun l ->
	match l with
	| [] -> 0
	| x :: [] -> x
	| x :: xm -> 
		let v = max xm in
		if x<v then v
		else x

let rec min : int list -> int
=fun l ->
	match l with
	| [] -> 0
	| x :: [] -> x
	| x :: xs ->
		let v = min xs in
		if x<v then x
		else v

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
	| Neg (fnt) -> (not (eval (fnt)))
	| Or (fnt, snd) -> ((eval (fnt)) || (eval (snd)))
	| And (fnt, snd) -> ((eval (fnt)) && (eval (snd)))
	| Imply (fnt, snd) ->
		if (eval (fnt) = false) then true
		else if (eval (snd) = true) then true
		else false
	| Equiv (fnt, snd) ->
		if (eval (fnt) = eval (snd)) then true
		else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
	match n1, n2 with
	| ZERO, ZERO -> ZERO
	| ZERO, SUCC(natval) -> n2
	| SUCC(natval), ZERO -> n1
	| SUCC(natn1), SUCC(natn2) -> SUCC( SUCC( natadd natn1 natn2))

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
	match n1, n2 with
	| ZERO, ZERO -> ZERO
	| ZERO, SUCC(natval) -> ZERO
	| SUCC(natval), ZERO -> ZERO
	| SUCC(natn1), SUCC(natn2) -> natadd (natmul natn1 n2) n2

