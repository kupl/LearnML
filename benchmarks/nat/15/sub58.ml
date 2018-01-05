type nat = ZERO | SUCC of nat;;

let rec natadd (x, y) = 
	match x with
	| ZERO -> y
	| SUCC a -> natadd (a, (SUCC y));;

let rec natmul (x, y) = 
	match x with
	| ZERO -> ZERO
	| SUCC a -> natadd (natmul (a, y), y);;
