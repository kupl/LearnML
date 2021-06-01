type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with 
    ZERO -> n2
    | SUCC x -> natadd x (SUCC n2) ;;
  
let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
    ZERO -> ZERO
    | SUCC x -> natadd (natmul x n2) n2 ;;