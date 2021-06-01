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

let rec calculate f2 = match f2 with
	|Num (i) -> i
	|Plus (e1, e2) -> calculate (e1) + calculate (e2)
	|Minus (e1, e2) -> calculate (e1) - calculate (e2)
in

match f with
	|True -> true
	|False -> false
	|Not (f)  -> if f=True then false else true
	|AndAlso (f1, f2) -> if (f1=True && f2=True) then true else false
	|OrElse (f1, f2) -> if (f1=True || f2=True) then true else false
	|Imply (f1, f2) -> if (f1=True && f2=False) then false else true
	|Equal (e1, e2) -> if calculate e1 = calculate e2
		then true else false
