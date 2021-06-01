(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
 | ZERO -> n2
 | SUCC n1 -> SUCC (natadd n1 n2)
 
let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
 | ZERO -> ZERO
 | SUCC ZERO -> n2
 | SUCC n1 -> natadd n2 ( natmul n1 n2 )

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
 | ZERO -> ZERO
 | SUCC ZERO -> SUCC ZERO
 |_ -> if n2 = ZERO then SUCC ZERO else SUCC ZERO 