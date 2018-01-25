(* Problem 1 *)
let rec pascal ( x , y ) =
	if y<0 then 0 else(*exception*)
	if y>x then 0 else 
	if y=0 then 1 else
	if y=x then 1 else
	pascal(x-1,y-1) + pascal(x-1,y);;

(* Problem 2 *)
let rec sigma test a b =
	if a > b then 0
	else (test a) + (sigma test (a+1) b);;

(* Problem 3 *)
let rec max l =
	match l with
		[]->0
		|hd::[] -> hd
		|hd::t1 -> if hd >= max t1 then hd else max t1;;

let rec min l =
	match l with
		[]->0
		|hd::[] -> hd
		|hd::t1 -> if hd <= min t1 then hd else min t1;;

(* Problem 4 *)
type formula =
	True
	|False
	|Neg of formula
	|Or of formula * formula
	|And of formula * formula
	|Imply of formula * formula
	|Equiv of formula * formula;;

let rec eval formula =
	match formula with
		True -> true
		|False -> false
		|Neg formula -> if eval formula = true then false else 
			true
		|Or (formula1 , formula2) -> if 
			eval formula1 = true then true else if 
			eval formula2 = true then true else false
		|And (formula1 , formula2) -> if 
			eval formula1 = false then false else if 
			eval formula2 = false then false else true
		|Imply (formula1 , formula2) -> if
			eval formula1 = true then if eval formula2 = false then false	else
			true else true
		|Equiv (formula1 , formula2) -> if
			eval formula1 = true then if eval formula2 = true then true else false else if
			eval formula2 = false then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat;;

let rec natadd nat1 nat2 =
	match nat1 with
		ZERO->nat2
		|SUCC natt -> SUCC (natadd natt nat2);;


let rec natmul nat1 nat2 =
		match nat1 with
			ZERO -> ZERO
			|SUCC nat3 -> natadd nat2 (natmul nat3 nat2) ;; 


