
(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
    match n2 with 
        ZERO-> n1
        | SUCC(n2minus1) -> natadd (SUCC(n1)) n2minus1
;;

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
    match n2 with
        ZERO -> ZERO
        | SUCC(n2minus1) -> natadd n1 (natmul n1 n2minus1)
;;

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> 
    match n2 with
        ZERO -> SUCC ZERO
        | SUCC(n2minus1) -> natmul n1 (natexp n1 n2minus1)
;;
