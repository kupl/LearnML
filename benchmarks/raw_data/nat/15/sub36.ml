type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
match n1 with
| ZERO -> n2
| SUCC m1 -> natadd m1 (SUCC n2) 

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
match n1 with
| ZERO -> ZERO
| SUCC ZERO  -> n2
| SUCC(SUCC m2 ) -> natadd (natmul (SUCC m2)  n2) n2;;
