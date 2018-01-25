type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun su1 su2->
		match su1, su2 with
		| ZERO, ZERO -> ZERO
		| SUCC(num1), ZERO -> su1
		| ZERO, SUCC(num2)-> su2
		| SUCC(num1), SUCC(num2) -> SUCC(SUCC(natadd num1 num2)

let rec natmul : nat -> nat -> nat
=fun su3 su4->
		match su3, su4  with
		|ZERO, ZERO -> ZERO
		|SUCC(num3), ZERO -> ZERO
		|ZERO, SUCC(num4) -> ZERO
		|SUCC(num3), SUCC(num4) -> natadd(natmul num3 su4) su4


