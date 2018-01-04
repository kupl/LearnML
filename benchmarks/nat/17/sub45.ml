type nat = ZERO
		 | SUCC of nat

let rec natadd : nat * nat -> nat = fun (a, b) ->
	match (a, b) with
	| (ZERO, _) -> b
	| (_, ZERO) -> a
	| (SUCC p, SUCC _) -> SUCC (natadd (p, b))

let rec natmul : nat * nat -> nat = fun (a, b) ->
	match (a, b) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (SUCC _, SUCC q) -> natadd (a, natmul (a, q))
