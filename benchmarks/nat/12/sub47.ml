type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) = 
	let rec cnt nat = 
		match nat with
		|ZERO -> 0
		|SUCC a -> 1 + (cnt a)
	in
	let rec mknat num = 
		match num with
		|0 -> ZERO
		|a -> SUCC (mknat (a-1))
	in
	let rec add (nat1, nat2) = 
		match (nat1,nat2) with
		|(ZERO,b) -> (cnt b)
		|(a,ZERO) -> (cnt a)
		|(a,b) -> (cnt a) + (cnt b)

	in
	mknat(add(nat1,nat2))

let rec natmul (nat1, nat2) = 
	let rec cnt nat = 
		match nat with
		|ZERO -> 0
		|SUCC a -> 1 + (cnt a)
	in
	let rec mknat num = 
		match num with
		|0 -> ZERO
		|a -> SUCC (mknat (a-1))
	in
	let rec mul (nat1, nat2) = 
	        match (nat1,nat2) with
	        |(ZERO,b) -> 0
	        |(a,ZERO) -> 0
	        |(a,b) -> (cnt a) * (cnt b)
	in
	mknat(mul(nat1,nat2))

