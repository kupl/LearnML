type nat = ZERO
|SUCC of nat

let rec natadd (a,b)=
	match (a,b) with
	|(_,ZERO)->a
	|(ZERO,_)->b
	|(SUCC t, _)-> SUCC (natadd (t, b))

let rec natmul (a,b)=
	match (a,b) with
	|(_,ZERO)->ZERO
	|(ZERO, _)->ZERO
	|(SUCC t,_)->natadd(natmul(t,b),b)
