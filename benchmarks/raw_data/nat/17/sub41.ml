(* problem 2 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> n1
| SUCC nat -> if nat = ZERO then SUCC n1
              else SUCC (natadd n1 nat);;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
| ZERO -> ZERO
| SUCC nat ->if nat = ZERO then n1
             else natadd n1 (natadd n1 nat);;

let natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
| ZERO -> SUCC ZERO
| SUCC nat -> if nat = ZERO then n1
              else natmul n1 (natmul n1 nat);;
