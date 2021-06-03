type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match n1 with
  SUCC (nat) -> natadd nat (SUCC(n2))
  |ZERO -> n2 ;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match n1 with
  SUCC (nat) -> natadd (natmul nat n2) n2
  |ZERO -> ZERO ;;

