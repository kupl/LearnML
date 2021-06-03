(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
| ZERO -> n2
| SUCC(nat) -> SUCC(natadd nat n2) 

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
| SUCC ZERO -> n1
| SUCC(nat) -> let inside = natmul n1 nat in natadd n1 inside
| ZERO -> ZERO

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
| ZERO -> ZERO
| _ -> match n2 with
      | SUCC ZERO -> n1
      | SUCC(nat) -> let inside = natexp n1 nat in natmul n1 inside
      | ZERO -> SUCC ZERO
