type nat = ZERO | SUCC of nat
let rec value (n: nat) : int =
	match n with
	| ZERO -> 0
	| SUCC x -> (value x)+1
let rec getnat (n: int) : nat =
	match n with
	| 0 -> ZERO
	| x -> SUCC (getnat (x-1)) 
	
let rec natadd (n1, n2) =
	getnat((value n1) + (value n2))
let rec natmul (n1, n2) =
	getnat((value n1)*(value n2))
	
