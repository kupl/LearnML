type nat = ZERO|SUCC of nat

let rec natadd a=function
	ZERO->a
	|SUCC b-> natadd(SUCC a) b
let rec natmul a b =match b with
	ZERO->ZERO
	|SUCC c-> natadd(natmul a c) a



	







