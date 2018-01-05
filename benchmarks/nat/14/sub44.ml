type nat = ZERO | SUCC of nat

let rec natadd: nat * nat -> nat =
	fun (n1, n2) ->
		match (n1, n2) with
		| (ZERO, ZERO) -> ZERO
		| (ZERO, SUCC n) -> SUCC (natadd (ZERO, n))
		| (SUCC n, ZERO) -> SUCC (natadd (n, ZERO))
		| (SUCC n1, SUCC n2) -> SUCC (SUCC (natadd (n1, n2)))

let rec natmul: nat * nat -> nat =
	fun (n1, n2) ->
		match (n1, n2) with
		| (SUCC n1, SUCC n2) -> SUCC (natadd (natmul (n1, n2), natadd (n1, n2)))
		| _ -> ZERO
