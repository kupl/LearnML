type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n2 with
  |ZERO -> n1
  |SUCC ZERO -> SUCC(n1)
  |SUCC(x) -> SUCC(natadd n1 x);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n2 with
  |ZERO -> ZERO
  |SUCC ZERO -> n1
  |SUCC(x) -> natadd (natmul n1 x) n1;;