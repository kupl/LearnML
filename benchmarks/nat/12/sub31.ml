type nat = ZERO | SUCC of nat
let rec natadd(a,b) = 
	match (a,b) with
	|(ZERO,t) -> t
	|(t,ZERO) -> t
	|(SUCC n1, SUCC n2) -> (SUCC (SUCC (natadd(n1,n2))))

let natmul(a,b) = 
	let rec getvalue natu = 
		match natu with
		|ZERO -> 0
		|SUCC k -> 1 + (getvalue k)
	in
	let rec makenat mul =
		if mul = 0 then ZERO
		else (SUCC (makenat (mul-1)))
	in
	makenat ((getvalue a) * (getvalue b))


