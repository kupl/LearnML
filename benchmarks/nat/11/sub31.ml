type nat = ZERO | SUCC of nat

let rec nattoint x =
	match x with
	ZERO -> 0
	|SUCC a -> (nattoint a)+1

let natadd (x, y) = (nattoint x)+(nattoint y)
let natmul (x, y) = (nattoint x)*(nattoint y)
