
(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> let rec na n1 n2 = match n2 with | ZERO -> n1 | SUCC nat -> na (SUCC n1) nat in na n1 n2

let natmul : nat -> nat -> nat 
= fun n1 n2 -> let n = n1 in let rec nm n1 n2 = match n2 with | ZERO -> ZERO | SUCC ZERO -> n1 | SUCC nat -> nm (natadd n1 n) nat in nm n1 n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> let n = n1 in let rec ne n1 n2 = match n2 with | ZERO -> (SUCC ZERO) | SUCC ZERO -> n1 | SUCC nat -> ne (natmul n1 n) nat in ne n1 n2
