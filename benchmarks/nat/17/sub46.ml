type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (a,b) ->
	match (a,b) with
	|(ZERO , _) -> b
	|(_, ZERO) -> a
	|(SUCC x, SUCC y) -> SUCC (SUCC (natadd (x,y)))

let rec natmul : nat * nat -> nat = fun (a,b) ->
	match (a,b) with
	|(ZERO , _) -> ZERO
	|(_ , ZERO) -> ZERO
	|(SUCC x, SUCC y) -> natadd (a, natmul(a,y))

(*
let a71 = natadd (SUCC ZERO, ZERO) 
let a72 = natadd (ZERO, ZERO) 
let a73 = natadd (SUCC ZERO, SUCC (SUCC ZERO)) 
let a74 = natmul (SUCC ZERO, ZERO) 
let a75 = natmul (ZERO, SUCC (SUCC (SUCC ZERO))) 
let a76 = natmul (SUCC (SUCC (SUCC (SUCC ZERO))), SUCC (SUCC (SUCC ZERO))) 
*)
