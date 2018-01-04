type nat = ZERO 
	| SUCC of nat

let rec natadd (a,b) =
	match (a,b) with
	|(ZERO,_) -> b
	|(_,ZERO) -> a
	|(SUCC x , SUCC y) -> (SUCC (SUCC (natadd (x,y))))

let rec natmul (a,b) = 
	let rec calc a =
		match a with
		|ZERO -> 0
		|(SUCC x) -> 1 + (calc x)
	in
	let rec makenat a =
		match a with
		|0 -> ZERO
		|a-> (SUCC (makenat (a-1)))
	in
	makenat ((calc a) * (calc b))
