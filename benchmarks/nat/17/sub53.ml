type nat = ZERO	| SUCC of nat

let rec natadd : (nat * nat) -> nat = fun (a, b) ->
	match (a, b) with
	| (ZERO, b) -> b
	| (a, ZERO) -> a
	| (SUCC(x), b) -> natadd(x, SUCC(b))

let rec natmul : (nat * nat) -> nat = fun (a, b) ->
	match (a, b) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (_, SUCC(bb)) -> natadd(a, natmul(a, bb))
