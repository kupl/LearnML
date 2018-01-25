(* Problem 1 *)
let pascal : int * int -> int
=fun (x,y) -> (* TODO *)
  let rec _pascal (a, b) =
	match (a, b) with
	|(0,0)-> 1
	|_ -> if (b < 0)||(a < b) then
			0
		  else
		    _pascal(a-1,b-1) + _pascal(a-1,b)
		in _pascal(x,y)
;;

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
	if a > b then
		0
	else
		(f a) + (sigma f (a+1) b)
;;

(*
	match a with
	|b -> 0
	|_ -> f a + (sigma f (a+1) b) 
	이렇게 하면 왜 안되는 건지
*)

(* Problem 3 *)
let rec max : int list -> int
=fun l ->
 match l with
 hd::tl -> if tl = [] then
			hd
		   else
			  let mx = max(tl) in
				if hd > mx then
					hd
				else
					mx
;;
			  
 

let rec min : int list -> int
=fun l -> 
 match l with
 hd::tl -> if tl = [] then
			hd
		   else
			  let mn = min(tl) in
				if hd < mn then
					hd
				else
					mn
;;



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
	|Neg (x) -> if eval(x) then
				false
			  else
				true
	|Or (x, y) -> if eval(x) || eval(y) then
					true
				  else
				    false
	|And (x,y) -> if eval(x) && eval(y) then
					true
				  else
				    false
	|Imply (x,y) -> if eval(x) && not(eval(y)) then
					   false
				    else
				       true
	|Equiv (x,y) -> if eval(x) = eval(y) then
						true
					else
						false
;;




(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
	match n1 with
	|ZERO -> n2
	|SUCC x -> SUCC (natadd x n2)
;;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
	if n2 = ZERO then 
		ZERO
	else
		match n1 with
		|ZERO -> ZERO
		|SUCC x -> if x = ZERO then 
					  n2
				   else
					  natadd (natmul x n2) n2
;;
	
	













