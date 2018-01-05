(*
  CSE 2012-11226 Kwak Jin Han
	exercise 1
 *)

type formula = TRUE
						 | FALSE
						 | NOT of formula
						 | ANDALSO of formula * formula		
						 | ORELSE of formula * formula
						 | IMPLY of formula * formula
						 | LESS of expr * expr
and expr = NUM of int
					| PLUS of expr * expr
					| MINUS of expr * expr

(* exprtoint : expr -> int *)
let rec exprtoint e =
	match e with
	| NUM num -> num
	| PLUS (x, y) -> exprtoint (x) + exprtoint (y)
	| MINUS (x, y) -> exprtoint (x) - exprtoint (y)

(* eval : formula -> bool *)
let rec eval f = 
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT pf -> not (eval pf)
	| ANDALSO (lf, rf) -> (eval lf) && (eval rf)
	| ORELSE (lf, rf) -> (eval lf) || (eval rf)
	| IMPLY (lf, rf) -> 
			if (eval lf) = false then true
			else (eval rf)
	| LESS (le, re) ->
			if (exprtoint le) < (exprtoint re) then true
			else false

(*
let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool (true = eval (NOT (NOT (NOT FALSE))));
print_bool (true = eval TRUE); 
print_bool (false = eval FALSE); 
print_bool (false = eval (NOT TRUE)); 
print_bool (true = eval (NOT FALSE)); 
print_bool (true = eval (ANDALSO (TRUE, TRUE))); 
print_bool (false = eval (ANDALSO (TRUE, FALSE))); 
print_bool (false = eval (ANDALSO (FALSE, TRUE))); 
print_bool (false = eval (ANDALSO (FALSE, FALSE))); 
print_bool (true = eval (ORELSE (TRUE, TRUE))); 
print_bool (true = eval (ORELSE (TRUE, FALSE))); 
print_bool (true = eval (ORELSE (FALSE, TRUE))); 
print_bool (false = eval (ORELSE (FALSE, FALSE))); 
print_bool (false = eval (IMPLY (TRUE, FALSE))); 
print_bool (true = eval (IMPLY (TRUE, TRUE))); 
print_bool (true = eval (IMPLY (FALSE, TRUE))); 
print_bool (true = eval (IMPLY (FALSE, FALSE))); 
print_bool (true = eval (LESS (NUM 3, NUM 5))); 
print_bool (false = eval (LESS (NUM 3, NUM 3))); 
print_bool (false = eval (LESS (NUM 3, NUM 1))); 
print_bool (false = eval 
		(LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)))); 
print_bool (true = eval 
		(LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13))))); 
print_bool (true = eval (ANDALSO (NOT (FALSE), NOT (NOT (NOT FALSE)))));	
print_bool (false = eval (ORELSE (NOT (NOT (NOT TRUE)), NOT (TRUE))));
*)
