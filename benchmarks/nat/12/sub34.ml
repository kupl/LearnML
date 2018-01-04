type nat = 
ZERO 
| SUCC of nat

let rec natadd (x, y) = 
	match x with
	| ZERO -> y
	| SUCC a -> ( match y with 
				| ZERO -> x
				| SUCC b -> SUCC ( natadd(a, y))
				)


let rec natmul (x, y) = 
	match x with 
	| ZERO -> ZERO
	| SUCC a -> ( match y with
				| ZERO -> ZERO
				| SUCC b -> natadd( y, (natmul(a, y)))
			)
