type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec eval : formula -> bool
= fun f -> match f with
	|True		-> true
	|False		-> false
	|Not x		-> if (eval x) = true then false else true
	|AndAlso (x,y)	-> 
		begin
		match eval x, eval y with
		|true,true -> true
		|_ -> false
		end
	|OrElse (x,y)	-> 
		begin
		match eval x, eval y with
		|false , false -> false
		|_ -> true
		end
	|Imply (x,y)	-> 
		begin
		match eval x, eval y with
		|true,false -> false
		|_ -> true
		end
	|Equal (x,y)	-> 
		let rec exp_eval e =
			begin
			match e with		
			|Num e1 -> e1	
			|Plus (e1,e2) -> (exp_eval e1) + (exp_eval e2)
			|Minus (e1,e2) -> (exp_eval e1) - (exp_eval e2)
			end
		in
		if exp_eval x = exp_eval y then true else false
