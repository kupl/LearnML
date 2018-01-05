type nat = ZERO | SUCC of nat

let rec natadd : (nat * nat) -> nat = function (a, b) ->
	match a with 
	| ZERO -> b
	| SUCC n -> SUCC (natadd (n, b))

let rec natmul : (nat * nat) -> nat = function (a, b) ->
	match a with
	| ZERO -> ZERO
	| SUCC n -> natadd (b, natmul (n, b))
