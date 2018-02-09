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
	let rec tf = function
		| True -> true
		| False -> false
		| Not(f) -> if tf f then false else true 
		| AndAlso(a,b) -> if (tf a && tf b)  then true else false 
		| OrElse(a,b) -> if (tf a || tf b)  then true else false 
		| Imply(a,b) -> if((tf a = true) && (tf b = false)) then false else true 
		| Equal(a,b) -> 
			let rec calc = function
			|Num(a) -> a
			|Plus(a,b) -> calc a + calc b
			|Minus(a,b) -> calc a - calc b in 
			if (calc a = calc b) then true else false 
		in tf f

