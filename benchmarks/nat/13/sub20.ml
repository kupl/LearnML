type nat = ZERO 
	| SUCC of nat
	

let rec eval n =
	match n with
	| ZERO -> 0
	| SUCC n -> 1 + (eval n)

let rec succ_n i =
	match i with
	|0 -> ZERO
	|_ -> SUCC (succ_n (i-1))

let natadd (n1,n2) = 
	succ_n ((eval n1) + (eval n2))

let natmul (n1,n2) = succ_n ((eval n1) * (eval n2))
