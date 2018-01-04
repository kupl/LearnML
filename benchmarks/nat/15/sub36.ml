type nat = ZERO | SUCC of nat

let minus_one n =
	match n with
		  ZERO -> ZERO
		| SUCC a -> a


let rec natadd (n1, n2) =
	match (n1, n2) with
		  (_, ZERO) -> n1
		| (ZERO, _) -> n2
		| _ -> SUCC ( SUCC ( natadd ((minus_one n1), (minus_one n2))))
		
	
let rec natmul (n1, n2) =
	match (n1, n2) with
		  (_, ZERO) -> ZERO
		| (ZERO, _) -> ZERO
		| (_, SUCC ZERO) -> n1
		| (SUCC ZERO, _) -> n2
		| _ -> natadd(n1, natmul (n1, (minus_one n2)))



