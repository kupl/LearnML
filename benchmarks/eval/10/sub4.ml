(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#2-3 *)

exception Error of string
exception DividedByZero

type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list

let rec eval e = match e with NUM v -> v
		| PLUS (e1, e2) -> (eval e1) + (eval e2)
		| MINUS (e1, e2) -> (eval e1) - (eval e2)
		| MULT (e1, e2) -> (eval e1) * (eval e2)
		| DIVIDE (e1, e2) -> if (eval e2) = 0 then raise DividedByZero else (eval e1) / (eval e2)
		| MAX [] -> 0
		| MAX (h::[]) -> eval h
		| MAX (h::m::t) -> if (eval h) > (eval m) then eval (MAX (h::t))
				else eval (MAX (m::t))
(*
(* test code *)
let expr0 = NUM 3
let expr1 = NUM 5
let expr2 = NUM (-4)
let expr3 = NUM 0

let expr4 = PLUS (expr0, expr1)
let expr5 = PLUS (expr2, expr3)
let expr6 = PLUS (expr0, expr2)

let expr7 = MINUS (expr0, expr1)
let expr8 = MINUS (expr1, expr2)

let expr9 = MULT (expr0, expr1)
let expr10 = MULT (expr1, expr2)
let expr11 = MULT (expr1, expr3)

let expr12 = DIVIDE (expr1, expr0)

let expr13 = PLUS (expr4, expr7)
let expr14 = MINUS (expr8, expr11)
let expr15 = MULT (expr5, expr8)

let expr16 = MAX [expr0; expr1; expr2; expr3; expr4; expr5; expr6; expr7; expr8; expr9; expr10; expr11; expr12; expr13; expr14; expr15]

let _ =
print_int (eval expr0);
print_newline();
print_int (eval expr1);
print_newline();
print_int (eval expr2);
print_newline();
print_int (eval expr3);
print_newline();
print_int (eval expr4);
print_newline();
print_int (eval expr5);
print_newline();
print_int (eval expr6);
print_newline();
print_int (eval expr7);
print_newline();
print_int (eval expr8);
print_newline();
print_int (eval expr9);
print_newline();
print_int (eval expr10);
print_newline();
print_int (eval expr11);
print_newline();
print_int (eval expr12);
print_newline();
print_int (eval expr13);
print_newline();
print_int (eval expr14);
print_newline();
print_int (eval expr15);
print_newline();
print_int (eval expr16);
print_newline()
*)
