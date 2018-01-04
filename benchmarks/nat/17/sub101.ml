type nat = ZERO | SUCC of nat
let rec natadd: nat * nat -> nat = fun(nat1,nat2) ->
(
	match nat1 with
		| ZERO -> nat2
		| SUCC(nat_) -> SUCC(natadd(nat_,nat2))
)
let rec natmul: nat * nat -> nat = fun(nat1,nat2) ->
(
	match nat1 with
		| ZERO -> ZERO
		| SUCC(nat_) -> natadd(nat2,natmul(nat_,nat2))
)
