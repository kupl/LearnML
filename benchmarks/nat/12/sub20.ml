(* ex7 *)
type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) = 
	match nat1 with
	  ZERO -> (match nat2 with
		    ZERO -> ZERO
		  | SUCC (n) -> SUCC (natadd(nat1, n)))
	| SUCC (n) -> SUCC (natadd(n, nat2))

let rec natmul (nat1, nat2) = 
	let rec nat2int (nat) = 
		match nat with
		  ZERO -> 0
		| SUCC (n) -> 1 + nat2int (n) in
	
	let rec int2nat (i) = 
		if i == 0 then ZERO
		else SUCC (int2nat (i - 1)) in
	
	int2nat ((nat2int nat1) * (nat2int nat2))
	