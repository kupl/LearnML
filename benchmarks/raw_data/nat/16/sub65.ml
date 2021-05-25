type nat =
	| ZERO
	| SUCC of nat

let rec nat_to_int: nat -> int
= fun n -> match n with
|ZERO -> 0
|SUCC x -> 1 + (nat_to_int x)

let rec int_to_nat: int -> nat
= fun c -> match c with
|0 -> ZERO
|x -> SUCC (int_to_nat (x-1))

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	let c1 = nat_to_int n1 and c2 = nat_to_int n2 in
		int_to_nat (c1 + c2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	let c1 = nat_to_int n1 and c2 =nat_to_int n2 in
		int_to_nat (c1 * c2)
