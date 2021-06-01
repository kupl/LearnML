type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n2 with
    | ZERO -> n1
    | SUCC (nat) -> natadd (SUCC (n1)) nat ;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n2 with
    | ZERO -> ZERO
    | SUCC (nat) -> natadd (natmul n1 nat) n1 ;;
