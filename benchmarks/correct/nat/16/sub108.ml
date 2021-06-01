type nat = 
	|ZERO
	|SUCC of nat


let rec natadd n1 n2 =
match n1 with
|ZERO -> n2
|SUCC a -> SUCC (natadd a n2)


let rec natmul n1 n2 =
match n2 with
|ZERO -> ZERO
|SUCC a -> natadd n1 (natmul n1 a)
