(*2011-11004 ³²À±¼® ¹®Á¦3*)

type nat = ZERO | SUCC of nat

let rec natadd (x, y) = 
	match (x, y) with
		(SUCC n1, SUCC n2) ->  natadd (SUCC (SUCC n1), n2)
		|(SUCC n1, ZERO) -> x
		|(ZERO, SUCC n2) -> y
		|(ZERO, ZERO)  -> ZERO
	
let rec natmul (x, y) =
	match (x, y) with	
		(SUCC n1, SUCC n2) ->  natadd (natmul (x,n2), x)
		|(SUCC n1, ZERO) -> ZERO
		|(ZERO, SUCC n2) -> ZERO
		|(ZERO, ZERO) -> ZERO	
			 		
