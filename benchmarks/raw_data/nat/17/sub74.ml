(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
               | ZERO -> n1
               | SUCC n3 -> natadd (SUCC (n1)) n3

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
               | ZERO -> ZERO
               | SUCC n3 -> natadd (natmul n1 n3) n1

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
               | ZERO -> (SUCC ZERO)
               | SUCC n3 -> match n3 with
                            | ZERO -> n1
                            | SUCC n4 -> natmul (natexp n1 n3) n1

