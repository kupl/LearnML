type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    ZERO -> n2
    |SUCC (nat0) -> SUCC (natadd nat0 n2)
      
let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
    ZERO -> ZERO
    |SUCC (nat0) -> natadd n2 (natmul nat0 n2) ;;