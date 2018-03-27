type nat = ZERO | SUCC of nat
(*  test value *)

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1, n2 with
| _ , ZERO -> n1
| ZERO , _ -> n2
| SUCC n1 , _ -> SUCC (natadd n1 n2)

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1, n2 with
| _ , ZERO -> ZERO
| ZERO , _ -> ZERO
| SUCC n1,_ -> natadd n2 (natmul n1 n2)
