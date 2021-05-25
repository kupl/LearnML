exception WrongInput
type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n2 with
ZERO -> n1 |
SUCC n3-> natadd (SUCC n1) n3;;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> let rec nataddmul : nat -> nat -> nat -> nat
=fun n1 n2 n3-> match n3 with
SUCC(ZERO) -> n1 |
SUCC n3 -> nataddmul (natadd n1 n2) n2 n3 in
if n1=ZERO||n2=ZERO then ZERO else nataddmul n1 n1 n2;;