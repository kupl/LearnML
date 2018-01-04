type nat = ZERO | SUCC of nat
let rec getnat x = match x with
	| ZERO -> 0
	| SUCC n -> 1+(getnat n)
let rec makenet n = 
	if n=0 then ZERO
	else SUCC (makenet (n-1))
let natadd (n1, n2)= makenet ((getnat n1)+(getnat n2))
let natmul (n1, n2)= makenet((getnat n1)*(getnat n2))

