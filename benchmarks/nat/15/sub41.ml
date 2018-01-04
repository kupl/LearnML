type nat = ZERO | SUCC of nat

let rec natadd (a, b) =  
	match b with
	| ZERO -> a
	| SUCC bb -> natadd (SUCC a, bb)
	
let rec natmul (a, b) = 
	match b with
	| ZERO -> ZERO
	| SUCC bb -> natadd (a, natmul (a, bb))