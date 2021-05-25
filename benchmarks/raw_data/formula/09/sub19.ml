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

let rec eval formulain= 
	let rec calc expin= 
		match expin with
			Num(x) -> x
			| Plus(x,y) -> (calc(x))+(calc(y))
			| Minus(x,y) -> (calc(x))-(calc(y))
	in
	match formulain with
		True -> true
		| False -> false
		| Not(interform) -> not (eval(interform))
		| AndAlso(interform1,interform2) -> eval(interform1) && eval(interform2)
		| OrElse(interform1,interform2) -> eval(interform1) || eval(interform2)
		| Imply(interform1,interform2) -> not (eval(interform1) && (not(eval(interform2))))
		| Equal(exp1,exp2) -> (calc(exp1)) = (calc(exp2))