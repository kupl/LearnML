type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  ZERO -> n2
  |SUCC (nat1) -> natadd nat1 (SUCC (n2));;
  
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  ZERO -> ZERO
  |_ -> match n2 with
    ZERO -> ZERO
    |SUCC ZERO -> n1
    |SUCC (nat2) -> natadd n1 (natmul n1 nat2);;