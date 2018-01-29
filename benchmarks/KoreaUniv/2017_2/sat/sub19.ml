(* problem 3*) 
type formula = 
	  True 
	| False 
	| Var of string 
	| Neg of formula 
	| And of formula * formula 
	| Or of formula * formula 
	| Imply of formula * formula 
	| Iff of formula * formula 
let sat : formula -> bool 
= fun f -> (* TODO *) 
match f with
|True -> true
|False -> false
|Var v -> bool_of_string v
|Neg(f1) -> if (f1 = True) then false else true 
|And(f1,f2) -> if (f1 = False) then false else if (f2 = True) then true else false
|Or(f1,f2) -> if (f1 = True) then true else if (f2 = True) then true else false 
|Imply(f1,f2) -> if (f1 = True && f2 = False) then false else true 
|Iff(f1,f2) -> if (f1 = f2) then true else false
