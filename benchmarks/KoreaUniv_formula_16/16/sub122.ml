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
	 
let rec getInt f = 
	match f with 
		|Num(n) -> n 
		|Plus(n1, n2) -> getInt(n1) + getInt(n2) 
		|Minus(n1, n2)-> getInt(n1) - getInt(n2);;


let rec eval f = 
	match f with
	|True   -> true
	|False  -> false
	|Not(a) -> if eval(a) then false else true
	|AndAlso (a,b) -> if eval(a) then begin if eval(b) then true else false end else false
	|OrElse  (a,b) -> if eval(a) then true else begin if eval(b) then true else false end
	|Imply   (a,b) -> if eval(a) then begin if eval(b) then true else false end else true
	|Equal   (a,b) -> if getInt(a)=getInt(b) then true else false;;
