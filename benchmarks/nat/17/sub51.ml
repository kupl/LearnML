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

let a = SUCC (SUCC (SUCC ZERO))
let b = SUCC (SUCC ZERO)
let expAdd = SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))
let expMul = SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))


let _ = print_endline (string_of_bool ((natadd (a, b)) = expAdd))
let _ = print_endline (string_of_bool ((natmul (a, b)) = expMul))
