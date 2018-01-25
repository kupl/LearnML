(* Problem 1 *)
let rec pascal (x,y) = if x = 0 then 1
		else if y = 0 then 1
		else if x = y then 1
		else pascal(x-1,y-1) + pascal(x-1,y)

(* Problem 2 *)
let rec sigma (test : int->int) (x : int) (y : int) : int  = 
				if x <= y then test x + (sigma test (x+1) y)
				else 0

(* Problem 3 *)
let rec max l = match l with
		| [] -> 0
		| hd::tl -> if hd > max tl then hd
			else max tl

let rec min l = match l with
		| [] -> 999999
		| hd::tl -> if hd < min tl then hd
			else min tl
(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval bl = match bl with
		| True -> true
		| False -> false
		| Neg a -> if (eval a) = true then false
			else true
		| Or (a,b) -> if (eval a) = true then true
			else if (eval b) = true then true
			else false
		| And (a,b) -> if (eval a) = false then false
			else if (eval b) = false then false
			else true
		| Imply (a,b) -> if (eval a) = true && (eval b) = false then false
				else true
		| Equiv (a,b) -> if (eval a) = (eval b) then true
				else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd n1 n2 = match n2 with
		| ZERO -> let rec nat n3 = match n3 with
					| ZERO -> ZERO
					| SUCC (n4) -> SUCC(nat n4) in nat n1
		| SUCC (n) -> SUCC (natadd n1 n)

let rec natmul n1 n2 = match n2 with
		| ZERO -> ZERO
		| SUCC (c) -> natadd n1 (natmul n1 c)

