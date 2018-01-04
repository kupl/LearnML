type nat = ZERO | SUCC of nat

let rec natadd: nat * nat -> nat = fun t ->
	match t with
	|(ZERO,b) -> b
	|(a,ZERO) -> a
	|(SUCC a,b) -> SUCC (natadd (a,b))

let rec natmul: nat * nat -> nat = fun t ->
	match t with
	|(ZERO,b) -> ZERO
	|(a,ZERO) -> ZERO
	|(SUCC a,b) -> natadd (b, natmul (a,b))
