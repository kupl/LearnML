type nat = ZERO | SUCC of nat;;
let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO;;
let rec natadd n1 n2 = match n2 with
ZERO -> n1
|SUCC (nat) -> SUCC (natadd n1 nat);;

let rec natmul : nat -> nat -> nat 
=fun n1 n2 -> ZERO;;
let rec natmul n1 n2 = match n2 with
| ZERO -> ZERO
| SUCC (nat) -> if n2 = ZERO then ZERO
else natadd n1 (natmul n1 nat);;