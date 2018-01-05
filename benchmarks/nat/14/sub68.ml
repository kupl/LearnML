type nat = ZERO | SUCC of nat

let rec natadd (n,m)=
    match n with
	| ZERO -> m
	| SUCC x -> natadd(x,SUCC m)

let rec natmul (n,m)=
    match n with
	| ZERO -> ZERO
	| SUCC x -> natadd(m,natmul(x,m))
