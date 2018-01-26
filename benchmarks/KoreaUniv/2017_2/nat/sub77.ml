(* problem 2 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->match n1 with
ZERO->n2
|SUCC(x)->SUCC(natadd x n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->match n1 with
ZERO->ZERO
|SUCC(x)->natadd(natmul x n2) (n2);;

let natexp : nat -> nat -> nat
=fun n1 n2 ->match n2 with
ZERO->SUCC(ZERO)
|SUCC(x)->natmul n1 (natmul n1 x);;
