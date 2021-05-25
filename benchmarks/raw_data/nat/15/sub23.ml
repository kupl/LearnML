type nat = ZERO | SUCC of nat 
let rec natadd : nat -> nat -> nat=fun n1 n2 -> 
match n2 with
ZERO -> n1
|SUCC n2 -> natadd (SUCC n1) n2;; 
let rec natmul : nat -> nat -> nat=fun n1 n2 -> 
match n2 with
ZERO -> ZERO
| SUCC y -> natadd (natmul n1 y) n1;;