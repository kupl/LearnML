type nat = ZERO | SUCC of nat

let rec natadd((a:nat), (b:nat)) =
	match a with
	| ZERO -> b
	| SUCC a' -> SUCC (natadd(a', b))

let rec natmul((a:nat), (b:nat)) =
	match (a,b) with
	|(ZERO, ZERO) -> ZERO
	|(ZERO, b) -> ZERO
	|(a, ZERO) -> ZERO
	|(SUCC ZERO, b) -> b
	|(a, SUCC ZERO) -> a
	|(SUCC a', b) -> natadd(natmul(a', b),b)  
