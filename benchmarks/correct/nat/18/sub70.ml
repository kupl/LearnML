type nat = ZERO | SUCC of nat ;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
    | ZERO -> n2
    | SUCC n1_prev -> SUCC (natadd n1_prev n2) ;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
    | ZERO -> ZERO
    | SUCC n1_prev -> natadd (natmul n1_prev n2) n2 ;;