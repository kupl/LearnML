type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> n2
| SUCC(n_minus_1) -> SUCC (natadd n_minus_1 n2)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
ZERO -> ZERO
| SUCC(n_minus_1) -> natadd n2 (natmul n_minus_1 n2)
