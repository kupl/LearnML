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
= fun f -> 
	let rec evalexp :  exp -> int = 
	fun f1 -> match f1 with
		| Num (a) -> a
		| Plus (a,b) -> evalexp (a) + evalexp (b)
		| Minus (a,b) -> evalexp (a) - evalexp (b) 
		in

		match f with
		| True -> true
		| False -> false
		| Not f1 -> if (f1=True) then false else true
		| AndAlso (f1,f2) -> if (f1=False) then false else if (f2=True) then true else false
		| OrElse (f1,f2) -> if (f1=True) then true else if (f2=True) then true else false
		| Imply (f1,f2) -> if (f1=True && f2=False) then false else true
		| Equal (a,b) -> if (evalexp a)=(evalexp b) then true else false;;

(* eval (Imply (Imply (True, False), True));;
eval (Equal (Num 1, Plus(Num 1, Num 2)));;
eval (Equal Plus(2,3), Plus(3,2));; *)


