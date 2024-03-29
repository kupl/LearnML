(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  | ZERO -> n1
  | SUCC n -> SUCC (natadd n1 n)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  | ZERO -> ZERO
  | SUCC n -> natadd n1 (natmul n1 n)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  | ZERO -> SUCC ZERO
  | SUCC n -> natmul n1 (natexp n1 n)

