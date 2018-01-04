type nat = ZERO | SUCC of nat
let pred : nat -> nat = fun nat -> 
	match nat with
		| ZERO -> ZERO
		| SUCC n -> n 
let succ : nat -> nat = fun nat -> SUCC nat
let rec natadd: nat * nat -> nat = 
	fun (x, y) -> if (x == ZERO) then y
					else if (x == SUCC ZERO) then SUCC y
					else natadd(pred x,succ y)
let rec natmul : nat * nat -> nat = 
	fun (x, y) -> if (x == ZERO) then ZERO
					else if (x == SUCC ZERO) then y
					else natadd(y, natmul(pred x, y))