type nat = ZERO | SUCC of nat

let rec nat2n nat = 
	match nat with
	| ZERO -> 0
	| SUCC s -> 1 + nat2n s

let rec n2nat n =
	match n with	
	| 0 -> ZERO
	| _ -> SUCC (n2nat (n-1))

let rec natadd (n1,n2) =
n2nat (nat2n n1 +nat2n n2)

let rec natmul (n1,n2) =
n2nat (nat2n n1 * nat2n n2)
