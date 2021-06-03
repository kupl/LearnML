(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with ZERO -> n2 | SUCC c -> SUCC (natadd c n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with ZERO -> ZERO | SUCC c -> natadd n2 (natmul c n2)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with ZERO -> SUCC ZERO | SUCC c -> natmul n1 (natexp n1 c)
