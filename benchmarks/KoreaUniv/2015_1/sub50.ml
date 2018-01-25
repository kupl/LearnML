(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if x = 0 || y = 0 then 1
	      else if x = y then 1 
              else (pascal(x-1,y-1) + pascal(x-1, y))

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a = 0 || b = 0 then 0
	      else if a = b then a 
	      else (f b + (sigma f a (b-1)))

(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
	| [] -> 0
	| [h] -> h
	| h :: t -> if h > (max t) then h
                    else (max t) 

let rec min : int list -> int
=fun l -> match l with
          | [] -> 0
          | [h] -> h
          | h :: t -> if h < (min t) then h
                      else (min t)

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
	| False -> false
	| Neg (a) -> if a = True then false else true   
	| Or (a, b) -> if(a = True || b = True) then true else false
	| And (a, b) -> if(a = True && b = True) then true else false
	| Imply (a, b) -> if a = True then true else false
	| Equiv (a, b) -> if((a = True && b = True) || (a = False && b = False)) then true else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
	| ZERO -> n2
	| SUCC m -> natadd m (SUCC n2)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
	| ZERO -> ZERO
	| SUCC m -> natadd n2 (natmul m n2)
