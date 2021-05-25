(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
    match n2 with
    |ZERO -> n1
    |SUCC ZERO -> SUCC(n1)
    |SUCC(n) -> SUCC(natadd n1 n);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
    match n2 with
    |ZERO -> ZERO
    |SUCC ZERO -> n1
    |SUCC(n) -> natadd (natmul n1 n) n1;;


let rec natexp : nat -> nat -> nat
= fun n1 n2 ->
    match n2 with
    |ZERO -> SUCC ZERO
    |SUCC ZERO -> n1
    |SUCC(n) -> natmul (natexp n1 n) n1;;