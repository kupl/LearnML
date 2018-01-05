(* 2004-11951 Noh, Soon Hyun *)

(* type given by TA *)
type expr = NUM of int
		  | PLUS of expr * expr
		  | MINUS of expr * expr
		  | MULT of expr * expr
		  | DIVIDE of expr * expr
		  | MAX of expr list

exception DividedByZero 

(* eval: expr -> int *)
let rec eval e =
	match e with
	| (NUM n) -> n
	| (PLUS (e1, e2)) -> (eval e1) + (eval e2)
	| (MINUS (e1, e2)) -> (eval e1) - (eval e2)
	| (MULT (e1, e2)) -> (eval e1) * (eval e2)
	| (DIVIDE (e1, e2)) 
		-> if (eval e2) = 0 then
			   raise (DividedByZero)
		   else
		   	   (eval e1) / (eval e2)
	| (MAX []) -> 0 
	(* Computation redundancy *)
	| (MAX (c::l)) -> if (eval c) > (eval (MAX l)) then (eval c)
					else (eval (MAX l))

(* Test functions
let test1 = (DIVIDE ((NUM 1), (NUM 2)))
let test2 = NUM 6
let test3 = NUM 2
let test4 = (MAX [test1;test2;test3])
let _ = print_int (eval test1); print_char '\n'
*)
