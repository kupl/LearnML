type nat = ZERO 
	 | SUCC of nat

let rec natadd (n1 , n2) = match (n1 , n2) with
	|(ZERO, n2) -> n2
	|(n1, ZERO) -> n1
	|(SUCC n3, SUCC n4) -> SUCC ( SUCC ( natadd (n3,n4) ) )

let rec natmul (n1 , n2) = match (n1, n2) with
	|(n1, ZERO) -> ZERO
	|(ZERO, n2) -> ZERO
	|(SUCC n3, n4) -> natadd ( natmul(n3,n4) , n4)
