type nat =
	| ZERO
	| SUCC of nat

let rec int_to_nat : int -> nat
= fun n -> match n with
| 0 -> ZERO
| _ -> SUCC (int_to_nat(n-1))

let rec nat_to_int : nat -> int
= fun n -> match n with
| ZERO -> 0
| SUCC n_ -> 1 + (nat_to_int n_)

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> int_to_nat ((nat_to_int n1) + (nat_to_int n2))

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> int_to_nat ((nat_to_int n1) * (nat_to_int n2))
