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
= fun f -> (* TODO *)

	match f with
	|True -> true
	|False -> false

	|Not( inNot ) -> if (eval inNot) then false else true
	
	|AndAlso(and1, and2) -> if (eval and1) && (eval and2) then true else false 
	
	|OrElse(or1, or2) -> if (eval or1) || (eval or2) then true else false
	
	|Imply(im1, im2) -> 
		(	match (eval im1) with
			|true -> if (eval im2) then true else false
			|false -> true 
		)

	|Equal(exp1, exp2) ->

		(*두가지의 exp를 받아서 같은지 다른지를 확인시켜주는 재귀함수*)
		let rec expcheck : exp -> int
		= fun e ->

			match e with
			|Num( inNum ) -> inNum
			|Plus(p1, p2) -> ( (expcheck p1) + (expcheck p2) )
			|Minus(m1, m2) -> ( (expcheck m1) - (expcheck m2) )

		in if (expcheck exp1)=(expcheck exp2) then true else false


