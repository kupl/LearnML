(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) ->	if y = 0 then 1 else   
				if x = y then 1 else
				pascal(x-1, y-1) + pascal (x-1, y);;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->	if a > b then 0 else
				(f a) + sigma f (a + 1) b;;

(* Problem 3 *)
let rec max : int list -> int
=fun l ->	match l with
			  [] -> 0
			| h::t ->	if t = [] then h else if h < (max t) then (max t) else h;;

let rec min : int list -> int
=fun l ->	match l with
			  [] -> 0
			| h::t ->	if t = [] then h else if h > (min t) then (min t) else h;;

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
=fun f ->	match f with
			  True -> true
			| False -> false
			| Neg f -> not (eval f)
			| Or (f, f') -> (eval f) || (eval f')
			| And (f, f') -> (eval f) && (eval f')
			| Imply (f, f') -> eval (Neg f) || (eval f')
			| Equiv (f, f') -> ((eval f) && (eval f')) || ((eval (Neg f)) && (eval (Neg f')));;  

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->	match n1 with
				  ZERO -> n2
				| SUCC (n1') -> SUCC (natadd n1' n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->	match n1 with
				  ZERO -> ZERO
				| SUCC (n1') -> natadd (natmul n1' n2) n2;;

