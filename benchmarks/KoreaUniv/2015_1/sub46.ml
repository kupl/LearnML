(* Problem 1 *)
exception Invalid_position;;

let rec pascal (x, y)  =
	if (y = 0 || x = y) then
		1
	else if (y < 0 || x < y || x < 0) then
		raise Invalid_position
	else
		(pascal(x-1, y) + pascal(x-1, y-1));;

print_endline (string_of_int (pascal (0, 0)));; (* 1 *)
print_endline (string_of_int (pascal (1, 0)));; (* 1 *)
print_endline (string_of_int (pascal (1, 1)));; (* 1 *)
print_endline (string_of_int (pascal (2, 1)));; (* 2 *)
print_endline (string_of_int (pascal (4, 2)));; (* 6 *)
print_endline "";;


(* Problem 2 *)
let rec sigma f a b =
	if ( a > b ) then
		0
	else if ( a = b ) then
		f a
	else
		f a + sigma f (a+1) b;;

print_endline (string_of_int (sigma (fun x -> x) 1 10));; (* 55 *)
print_endline (string_of_int (sigma (fun x -> x*x) 1 7));; (* 140 *)
print_endline "";;


(* Problem 3 *)
exception Empty_list;;

let rec max l = 
	match l with
	|[] -> raise Empty_list
	|h::[] -> (0+h) (* to make type int *)
	|h::t -> if (h > max t) then h else max t;;

let rec min l =
	match l with
	|[] -> raise Empty_list
	|h::[] -> (0+h) (* to make type int *)
	|h::t -> if (h < max t) then h else max t;;

print_endline (string_of_int (max [1; 3; 5; 2]));; (* 5 *)
print_endline (string_of_int (min [1; 3; 2]));; (* 1 *)
print_endline ("");


(* Problem 4 *)
type formula =
	True
	| False
	| Neg of formula
	| Or of formula * formula
	| And of formula * formula
	| Imply of formula * formula
	| Equiv of formula * formula;;

let eval f =
	match f with
	True -> true
	| False -> false
	| Neg x -> if (x = True) then false else true
	| Or (x, y) -> if (x = True || y = True) then true else false
	| And (x, y) -> if (x = True && y = True) then true else false
	| Imply (x, y) -> if (x = True && y = False) then false else true
	| Equiv (x, y) -> if (x = y) then true else false;;


print_endline (string_of_bool (eval True));; (* true *)
print_endline (string_of_bool (eval False));; (* false *)
print_endline (string_of_bool (eval (Neg(True))));; (* false *)
print_endline (string_of_bool (eval (Neg(False))));; (* true *)
print_endline (string_of_bool (eval (Or(True, True))));; (* true *)
print_endline (string_of_bool (eval (Or(True, False)))); (* true *)
print_endline (string_of_bool (eval (Or(False, True))));; (* true *)
print_endline (string_of_bool (eval (Or(False, False))));; (* false *)
print_endline (string_of_bool (eval (And(True, True))));; (* true *)
print_endline (string_of_bool (eval (And(True, False)))); (* false *)
print_endline (string_of_bool (eval (And(False, True))));; (* false *)
print_endline (string_of_bool (eval (And(False, False))));; (* false *)
print_endline (string_of_bool (eval (Imply(True, True))));; (* true *)
print_endline (string_of_bool (eval (Imply(True, False)))); (* false *)
print_endline (string_of_bool (eval (Imply(False, True))));; (* true *)
print_endline (string_of_bool (eval (Imply(False, False))));; (* true *)
print_endline (string_of_bool (eval (Equiv(True, True))));; (* true *)
print_endline (string_of_bool (eval (Equiv(True, False)))); (* false *)
print_endline (string_of_bool (eval (Equiv(False, True))));; (* false *)
print_endline (string_of_bool (eval (Equiv(False, False))));; (* true *)
print_endline "";


(* Problem 5 *)
type nat = ZERO | SUCC of nat;;

let rec natadd a b =
	match a with
	ZERO -> b
	| SUCC (x) -> natadd x (SUCC(b));;

let rec natmul a b =
	match a with
	ZERO -> ZERO
	| SUCC ZERO -> b
	| SUCC (x) -> natadd (natmul x b) b;;

let rec string_of_nat_ a b = (* val string_of_nat_ : nat -> int -> string = <fun> *)
	match a with
	ZERO -> string_of_int b
	| SUCC (x) -> string_of_nat_ x (b+1)
and string_of_nat x = (* val string_of_nat : nat -> string = <fun> *)
	string_of_nat_ x 0;;
(* string_of_nat function convert nat to int *)

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
natmul two three;;
natadd two three;;

print_endline (string_of_nat (two));; (* 2 *)
print_endline (string_of_nat (three));; (* 3 *)
print_endline (string_of_nat (natmul two three));; (* 6 *)
print_endline (string_of_nat (natadd two three));; (* 5 *)

