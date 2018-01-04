type nat = ZERO
	| SUCC of nat;;

let rec natadd(a,b)=
	match a with
	| ZERO -> b
	| SUCC tail -> natadd(tail, SUCC b);;

let rec natmul(a,b)=
	match a with
	| ZERO -> ZERO
	| SUCC ZERO -> b
	| SUCC tail -> natadd(b, natmul(tail, b));;