type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
		| ZERO -> n2
		| SUCC x -> SUCC (natadd x n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let rec mul num = match num with
		| 0 -> ZERO
		| _ -> SUCC (mul (num - 1))
	in let rec cal a = match a with
		| ZERO -> 0
		| SUCC a -> 1 + cal a
	in mul ((cal n1) * (cal n2))
