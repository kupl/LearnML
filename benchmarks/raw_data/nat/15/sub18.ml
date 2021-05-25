type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
match n1 with
|ZERO -> n2
|SUCC x -> SUCC(natadd n2 x);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
match n1 with
|ZERO -> ZERO
|SUCC x -> natadd n2 (natmul n2 x);;
