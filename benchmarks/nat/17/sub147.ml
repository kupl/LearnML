(*2016-11690*)
type nat = ZERO | SUCC of nat

let rec natadd (nat1,nat2) =
	match nat1 with
	|ZERO -> nat2
	|SUCC num -> natadd(num,SUCC nat2)

let rec natmul (nat1,nat2) =
	match nat1 with
	|ZERO -> ZERO
	|SUCC num -> natadd(nat2,natmul(num,nat2))