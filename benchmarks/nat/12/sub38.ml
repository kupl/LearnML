type nat = ZERO | SUCC of nat

let rec natadd (a,b) = 
	match (a,b) with
	  (_,ZERO) -> a
	| (ZERO,_) -> b
	| (SUCC a,SUCC b) -> natadd(a,SUCC (SUCC b))


let rec natmul (a,b) =
	match (a,b) with	
		  (_,ZERO) -> ZERO
		| (ZERO,_) -> ZERO
		| (SUCC a, SUCC b) -> natadd(natmul(a, SUCC b),SUCC b)
