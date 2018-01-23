(*Lee Seok Jin 2013-11417 CSE HW1_4*)

type nat = ZERO | SUCC of nat

let rec natadd((op1:nat), (op2:nat)):nat=
	match op1 with
	| ZERO -> op2
	| SUCC _op1 -> SUCC(natadd(_op1, op2))

let rec natmul((op1:nat),(op2:nat)):nat=
	match op1 with
	| ZERO -> ZERO
	| SUCC _op1 -> natadd(op2, (natmul(_op1, op2))) 
