(*
  CSE 2012-11226 Kwak Jin Han
	exercise 1
 *)

type formula = True
						 | False
						 | Not of formula
						 | AndAlso of formula * formula		
						 | OrElse of formula * formula
						 | Imply of formula * formula
						 | Equal of exp * exp
and exp = Num of int
					| Plus of exp * exp
					| Minus of exp * exp

(* exptoint : exp -> int *)
let rec exptoint e =
	match e with
	| Num num -> num
	| Plus (x, y) -> exptoint (x) + exptoint (y)
	| Minus (x, y) -> exptoint (x) - exptoint (y)

(* eval : formula -> bool *)
let rec eval f = 
	match f with
	| True -> true
	| False -> false
	| Not pf -> not (eval pf)
	| AndAlso (lf, rf) -> (eval lf) && (eval rf)
	| OrElse (lf, rf) -> (eval lf) || (eval rf)
	| Imply (lf, rf) -> 
			if (eval lf) = false then true
			else (eval rf)
	| Equal (le, re) ->
			if (exptoint le) = (exptoint re) then true
			else false

(*
let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool (true = eval (Not (Not (Not False))));
print_bool (true = eval True); 
print_bool (false = eval False); 
print_bool (false = eval (Not True)); 
print_bool (true = eval (Not False)); 
print_bool (true = eval (AndAlso (True, True))); 
print_bool (false = eval (AndAlso (True, False))); 
print_bool (false = eval (AndAlso (False, True))); 
print_bool (false = eval (AndAlso (False, False))); 
print_bool (true = eval (OrElse (True, True))); 
print_bool (true = eval (OrElse (True, False))); 
print_bool (true = eval (OrElse (False, True))); 
print_bool (false = eval (OrElse (False, False))); 
print_bool (false = eval (Imply (True, False))); 
print_bool (true = eval (Imply (True, True))); 
print_bool (true = eval (Imply (False, True))); 
print_bool (true = eval (Imply (False, False))); 
print_bool (true = eval (Equal (Num 3, Num 5))); 
print_bool (false = eval (Equal (Num 3, Num 3))); 
print_bool (false = eval (Equal (Num 3, Num 1))); 
print_bool (false = eval 
		(Equal (Plus (Num 3, Num 4), Minus (Num 5, Num 1)))); 
print_bool (true = eval 
		(Equal (Plus (Num 10, Num 12), Minus (Num 10, Num (-13))))); 
print_bool (true = eval (AndAlso (Not (False), Not (Not (Not False)))));	
print_bool (false = eval (OrElse (Not (Not (Not True)), Not (True))));
*)
