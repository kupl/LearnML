type nat = ZERO | SUCC of nat

let rec nattoint nat =
	match nat with
	| ZERO -> 0
	| SUCC (nature) -> 1+ nattoint(nature)
let rec inttonat num =
	if num = 0 then ZERO else SUCC(inttonat(num-1))

let natadd(nat1,nat2) =
	inttonat(nattoint(nat1)+nattoint(nat2))
let natmul(nat1,nat2) =
	inttonat(nattoint(nat1)*nattoint(nat2))
	
