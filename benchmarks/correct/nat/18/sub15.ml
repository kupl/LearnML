type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  match n1 with 
    ZERO -> n2
    |SUCC(h) -> natadd h (SUCC(n2));;



let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  match n2 with
    ZERO -> ZERO
    |SUCC(h) -> natadd n1 (natmul n1 h);;
    