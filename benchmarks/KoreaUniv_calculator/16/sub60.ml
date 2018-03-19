
	
	type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

	let rec sigma s b f = if b=s-1 then 0 
												else ((sigmacal f b))+sigma s (b-1) f
	
	and sigmacal : exp->int->int
	= fun f a ->  match f with
		|X->a
		|INT n -> n
		|ADD (n1,n2) -> (sigmacal n1 a)+(sigmacal n2 a)
    |SUB (n1,n2) -> (sigmacal n1 a)-(sigmacal n2 a)
    |MUL (n1,n2) -> (sigmacal n1 a)*(sigmacal n2 a)
    |DIV (n1,n2) -> (sigmacal n1 a)/(sigmacal n2 a)
    |SIGMA (n1,n2,f) -> sigma (sigmacal n1 a) (sigmacal n2 a) f
		 
  let rec  calculator : exp -> int
  = fun exp -> match exp with
		|X-> raise (Failure "Input is not correct")
		|INT n -> n
		|ADD (n1,n2) -> (calculator n1)+(calculator n2)
		|SUB (n1,n2) -> (calculator n1)-(calculator n2)
		|MUL (n1,n2) -> (calculator n1)*(calculator n2)
		|DIV (n1,n2) -> (calculator n1)/(calculator n2)
		|SIGMA (n1,n2,f) -> sigma (calculator n1) (calculator n2) f