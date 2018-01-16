(* 4190.310 Programming Language 			*
 * Homework #2 - Exercise 3 (계산해줘!)		*
 * 2008-11744 Jongwook Choi 				*)

type expr 	= NUM of int
			| PLUS of expr * expr
			| MINUS of expr * expr
			| MULT of expr * expr
			| DIVIDE of expr * expr
			| MAX of expr list
;;


let rec eval (e : expr) =
	match e with
		  NUM x -> x
		| PLUS (l, r) -> (eval l) + (eval r)
		| MINUS (l, r) -> (eval l) - (eval r)
		| MULT (l, r) -> (eval l) * (eval r)
		| DIVIDE (l, r) -> (eval l) / (eval r)
		| MAX lst ->
			if (List.length lst) = 0 then 0
			else List.fold_right (fun e' v -> max (eval e') v) lst (min_int) 
;;
