exception TODO


type nat = ZERO | SUCC of nat

let rec natadd (a,b): nat =
	match a,b with
	| ZERO,_ -> b
	| _,ZERO -> a
	| (SUCC sub_a),_ -> natadd ((sub_a),(SUCC b))

let rec mul (a: nat) (original_b: nat) (b: nat): nat =
	match a with
	| ZERO -> ZERO
	| (SUCC ZERO) -> b
	| (SUCC sub_a) -> mul (sub_a) (original_b) (natadd (original_b, b))

let rec natmul (a, b): nat =
	match a,b with
	| ZERO,_ -> ZERO
	| _,ZERO -> ZERO
	| (SUCC ZERO),_ -> b
	| _,(SUCC ZERO) -> a
	| _,_ -> mul a b b




