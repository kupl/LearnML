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

let rec cal num = match num with
	Num  i -> Num i
	|Plus (a,b)-> (match a, b with
									 Num a2, Num b2 -> Num(a2 + b2)
									|_,_ -> Plus(cal a, cal b))													
	|Minus (a,b) -> (match a, b with
									Num a2, Num b2 -> Num (a2 - b2)
									|_,_ -> Minus(cal a, cal b)) 


let rec eval : formula -> bool
= fun f -> match f with
	True-> true
	|False-> false
	|Not x -> if (eval(x) = true) then false else true
	|AndAlso (x,y) -> (eval x) && (eval y)
	|OrElse (x,y) -> (eval x) || (eval y)
	|Imply (x,y) -> if((eval x = true) &&(eval y = false)) then false else true
	|Equal (x,y) ->(match x, y with
											Num x2, Num y2-> if x2 =y2 then true else false
											|_,_ -> eval (Equal (cal x, cal y)))


		



