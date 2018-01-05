type nat = ZERO | SUCC of nat

let rec nat_eval n =
	match n with ZERO -> 0
	| SUCC m -> 1 + nat_eval m

let rec nat_gen n =
	if n = 0 then ZERO
	else SUCC (nat_gen (n-1))

let natadd (a,b) =
	nat_gen(nat_eval a + nat_eval b)

let natmul (a,b) =
	nat_gen(nat_eval a * nat_eval b)

