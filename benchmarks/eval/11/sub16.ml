(* 2008-11874 Lee, Sujee *)
(* EXERCISE 3 *)		
type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

exception CANNOTeval of string

let rec eval exp = (* eval: expr -> int *)
	match exp with
		| NUM i -> i
		| PLUS(e1,e2) -> (eval e1) + (eval e2)
		| MINUS(e1,e2) -> (eval e1) - (eval e2)
		| MULT(e1,e2) -> (eval e1) * (eval e2)
		| DIVIDE(e1,e2) -> if (eval e2)=0 then raise (CANNOTeval "divide by 0")
											else (eval e1) / (eval e2)
		| MAX exl -> 
			if (exl=[]) then 0
			else List.hd (List.rev (List.sort compare (List.map eval exl)))